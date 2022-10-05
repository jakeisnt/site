(load "~/quicklisp/setup.lisp")
(load "./parser.lisp")
(load "./defs.lisp")

(ql:quickload :spinneret)
(ql:quickload 'css-lite)
(ql:quickload :parenscript)

(defpackage html
  (:use :cl :spinneret))

(defun render-text-elem (txt)
  "Render a text element."
  (spinneret::with-html
    (cond
      ((stringp txt) (:span txt))
      ((defs::link-p txt) (:a :href (defs::link-url txt) (defs::link-title txt)))
      (t "fell through the cracks"))))

(defmacro render-header-body (header)
  `(loop for node in (parser::header-body ,header)
        collect (render-org-node node)))


(defmacro render-text-body (body-list)
  `(loop for txt in ,body-list
         collect (render-text-elem txt)))

(defun render-org-node (node)
  "Render an org-mode node as HTML."
  (spinneret::with-html
    (cond
      ((parser::header-p node)
       (:section
        (cons
         (:h2 (parser::header-title node))
         (render-header-body node))))
      ((parser::text-p node)
       (:p (render-text-body (parser::text-body node))))
      ((parser::bullet-p node)
       (:ul (:li (render-text-body (parser::bullet-body node)))))
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

;; (render-org-file "../README.org")
