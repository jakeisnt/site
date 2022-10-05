(load "~/quicklisp/setup.lisp")
(load "./parser.lisp")
(load "./defs.lisp ")

(ql:quickload :spinneret)
(ql:quickload 'css-lite)
(ql:quickload :parenscript)

(defpackage html
  (:use :cl :spinneret))

(defun render-text-elem (txt)
  "Render a text element."
  (print txt)
  (spinneret::with-html
    (cond
      ((stringp txt) (:span txt))
      ((link-p txt) (:a :href (link-url txt) (link-title txt)))
      (t "fell through the cracks"))))

(defun render-org-node (node)
  "Render an org-mode node as HTML."
  (spinneret::with-html
      (cond
        ((parser::header-p node)
         (cons
          (:h2 (parser::header-title node))
          (loop for node in (parser::header-body node)
                collect (render-org-node node))))
        ((parser::text-p node)
         (:p (loop for txt in (parser::text-body node)
                   collect (render-text-elem txt))))
        ((parser::bullet-p node) (:ul (:li (parser::bullet-body node))))
        ((parser::code-block-p node) (:code (parser::code-block-body node))))))

(defun render-org (org)
  "Render an org file struct as an html page"
  (spinneret::with-html
    (:html
     :lang "en-us"
     (:head
      (:title (concatenate 'string (parser::file-title org) " | Jake Chvatal")))
     (:body
      (when (parser::file-title org)
        (:h1 (parser::file-title org)))
      (loop for node in (parser::file-body org)
            collect (render-org-node node))))))

(defun render-org-file (fname)
  (render-org (parser::parse fname)))
