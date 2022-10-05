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

(defmacro render-text-body (body-list)
  `(loop for txt in ,body-list
         collect (render-text-elem txt)))

(defmacro render-header (header)
  `(let ((title (parser::header-title ,header)))
    (spinneret::with-html
      (case (parser::header-rank ,header)
        (0 (:h2 title))
        (1 (:h3 title))
        (2 (:h4 title))
        (otherwise (:h5 title))))))

(defmacro render-header-body (header)
  `(loop for node in (parser::header-body ,header)
        collect (render-org-node node)))

(defun render-org-node (node)
  "Render an org-mode node as HTML."
  (spinneret::with-html
    (cond
      ((parser::header-p node)
       (:section
        (cons
         (render-header node)
         (render-header-body node))))
      ((parser::text-p node)
       (:p (render-text-body (parser::text-body node))))
      ((parser::bullet-p node)
       (:ul (:li (render-text-body (parser::bullet-body node)))))
      ((parser::code-block-p node) (:pre (:code
                                          :class (concatenate 'string "language-" (parser::code-block-lang node))
                                          (parser::code-block-body node)))))))

(defun render-org (org)
  "Render an org file struct as an html page"
  (spinneret::with-html
    (:html
     :lang "en-us"
     (:head
      (:title (concatenate 'string (parser::file-title org) " | Jake Chvatal"))
      (:link :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/styles/default.min.css")
      (:script :src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/highlight.min.js")
      (:script "hljs.highlightAll();"))
     (:body
      (when (parser::file-title org)
        (:h1 (parser::file-title org)))
      (loop for node in (parser::file-body org)
            collect (render-org-node node))))))

(defun render-org-file (fname)
  (render-org (parser::parse fname)))

;; (render-org-file "../README.org")
