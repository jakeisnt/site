(load "~/quicklisp/setup.lisp")
(load "./src/org/parser.lisp")
(load "./src/org/defs.lisp")
(load "./src/util.lisp")
(load "./src/components.lisp")

(ql:quickload :spinneret)
(ql:quickload 'css-lite)
(ql:quickload :parenscript)

(defpackage org-html
  (:use :cl :spinneret))

(in-package org-html)

;; three cases:
;; `id:`: this should find the file with the given id and link to it.
;; `file:`: this should get the file at the path and link to it.
;;          this could, unfortunately, be a relative path.
;; `https://`: leave it alone. this link works.
(defun convert-link (txt)
  (spinneret::with-html
    (let* ((url (defs::link-url txt))
          (title (or (defs::link-title txt) url))
          (is-internal
            (or
             (util::string-prefixesp url  "id:")
             (util::string-prefixesp url "file:"))))
      (:a
       :href url
       :class (if is-internal "internal" "external")
       (if is-internal
           (concatenate 'string "{" title "}")
           (concatenate 'string "[" title "]"))))))

(defun render-text-elem (txt)
  "Render a text element."
  (spinneret::with-html
    (cond
      ((stringp txt) (:span txt))
      ((defs::link-p txt) (convert-link txt))
      ((defs::bold-p txt) (:b (defs::bold-text txt)))
      ((defs::ital-p txt) (:i (defs::ital-text txt)))
      ((defs::verb-p txt) (:pre txt))
      (t "fell through the cracks"))))

(defmacro render-text-body (body-list)
  "Render the body of a text element."
  `(loop for txt in ,body-list
         collect (render-text-elem txt)))

(defmacro header-head (header)
  "Render the header of a text element."
  `(let ((title (parser::header-title ,header)))
     (spinneret::with-html
       (case (parser::header-rank ,header)
         (0 (:h2 title))
         (1 (:h3 title))
         (2 (:h4 title))
         (otherwise (:h5 title))))))

(defmacro header-body (header)
  `(loop for node in (parser::header-body ,header)
         collect (render-org-node node)))

(defun header (node)
  (spinneret::with-html
    (:section
     (cons
      (header-head node)
      (header-body node)))))

(defun text (node)
  (spinneret::with-html
    (:p (render-text-body (parser::text-body node)))))

(defun bullet (node)
  (spinneret::with-html
    (:ul (:li (render-text-body (parser::bullet-body node))))))

(defun code-block (node)
  (spinneret::with-html
    (:pre
     (:code
      :class (concatenate 'string "language-" (parser::code-block-lang node))
      (parser::code-block-body node)))))

(defun render-org-node (node)
  "Render an org-mode node as HTML."
  (spinneret::with-html
    (cond
      ((parser::header-p node) (header node))
      ((parser::text-p node) (text node))
      ((parser::bullet-p node) (bullet node))
      ((parser::code-block-p node) (code-block node))
      (t nil))))

(defparameter *url* "/home/jake/site/docs") ;; TODO may not be good practice

(defun render-org (org pathlist)
  "Render an org file struct as an html page"
  (let ((title (parser::file-title org))
        (body (parser::file-body org)))
    (spinneret::with-html
        (:html
         :lang "en-us"
         (:head
          (:title (concatenate 'string (or title "") " | Jake Chvatal"))
          (:link :rel "stylesheet" :href (concatenate 'string *url* "/style.css"))
          (:link :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/styles/default.min.css")
          (:script :src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/highlight.min.js")
          (:script "hljs.highlightAll();"))
         (:body
          (:main
           (comp::sidebar pathlist)
           (when title (:h1 title))
           (loop for node in body
                 collect (render-org-node node))))))))

(defun render-org-file (fname pathlist)
  (spinneret::with-html-string (render-org (parser::parse fname) pathlist)))

;; (util::write-file "./test.html" (render-org-file "./README.org"))
