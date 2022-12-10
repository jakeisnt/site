(load "~/quicklisp/setup.lisp")
(load "~/site/src/org/ast.lisp")
(load "~/site/src/util.lisp")
(load "~/site/src/components.lisp")
(load "~/site/src/path.lisp")

(ql:quickload :spinneret)
(ql:quickload 'css-lite)

(defpackage org-html
  (:use :cl))

(in-package org-html)

;; TODO: three cases:
;; `id:`: this should find the file with the given id and link to it.
;; `file:`: this should get the file at the path and link to it.
;;          this could, unfortunately, be a relative path.
;; `https://`: leave it alone. this link works.
;; There may be other cases. Handle those!
;; I also might want to allow for opening internal links
;;   as separate panes on the same page rather than as new pages,
;;   but only on the desktop. How do I do this?

;; NOTE: we may have to do more work here.

(defun internal-urlp (url)
  "Is the URL internal (the file can be found local to the wiki)?"
  (or
   (util::string-prefixesp url  "id:")
   (util::string-prefixesp url "file:")))

(defun path->url (path)
  "Change a file path to its final html location."
  (fpath::change-file-path path common-lisp-user::*wiki-location* common-lisp-user::*site-location*))

(defun convert-url (url)
  "Convert a local file link to its corresponding wiki path."
  (cond
    ;; It's a link to the web already
    ((util::string-prefixesp url "http") url)
    ;; It's a link to the final site (risky, but necessary?)
    ((util::string-prefixesp url "/") url)
    ;; its external in the first place; return og url
    ((util::string-prefixesp url "file:")
     (path->url
      (util::without-prefix url "file:")))
    (t (error
        (concatenate 'string "We don't support '" url "' kinds of local links! Remove them, silly.")))))

(defun convert-link (txt)
  "Convert a link to an HTML link."
  (spinneret::with-html
      (let* ((url (convert-url (ast::link-url txt)))
             (title (or (ast::link-title txt) url))
             (is-internal (internal-urlp url)))
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
      ((ast::link-p txt) (convert-link txt))
      ((ast::bold-p txt) (:b (ast::bold-text txt)))
      ((ast::ital-p txt) (:i (ast::ital-text txt)))
      ((ast::verb-p txt) (:pre txt))
      (t (progn
           (print txt)
           (error "We don't support this type of text element yet : ("))))))

(defmacro render-text-body (body-list)
  "Render the body of a text element."
  `(loop for txt in ,body-list
         collect (render-text-elem txt)))

(defmacro header-head (header)
  "Render the header of a text element."
  `(let ((title (ast::header-title ,header)))
     (spinneret::with-html
       (case (ast::header-rank ,header)
         (0 (:h2 title))
         (1 (:h3 title))
         (2 (:h4 title))
         (otherwise (:h5 title))))))

(defmacro header-body (header)
  `(loop for node in (ast::header-body ,header)
         collect (render-node node)))

(defun header (node)
  (spinneret::with-html
    (:section
     (cons
      (header-head node)
      (header-body node)))))

(defun text (node)
  (spinneret::with-html
    (:p (render-text-body (ast::text-body node)))))


(defun bullet (node)
  (spinneret::with-html
    (:li (render-text-body (ast::bullet-body node)))))

(defun bullet-group (node)
  (spinneret::with-html
    (:ul (loop for bullet in (ast::bullet-group-body node)
               collect (bullet bullet)))))

(defun render-node (node)
  "Render a node as HTML."
  (spinneret::with-html
    (cond
      ((ast::header-p node) (header node))
      ((ast::text-p node) (text node))
      ((ast::bullet-group-p node) (bullet-group node))
      ((ast::code-block-p node) (code-block node))
      ((ast::quote-block-p node) (quote-block node))
      (t node))))

(defun render-nodelist (nodes)
  "Render a list of nodes as html"
  (spinneret::with-html
    (loop for node in nodes
          collect (render-node node))))

(defun code-block (node)
  (spinneret::with-html
    (:pre
     (:code
      :class (concatenate 'string "language-" (ast::code-block-lang node))
      (ast::code-block-body node)))))

(defun quote-block (node)
  (spinneret::with-html
    (:blockquote
     :class "article-quote"
     (:p (ast::quote-block-body node))
     (:cite (render-nodelist (ast::quote-block-author node))))))

;; macro: https://github.com/ruricolist/spinneret
;; TODO: add edit icon.  this can just take to github page,
;; or open up buffer and take to github on save,
;; or something else - maybe my own endpoint? just has to be able to give user ability to contribute.
(defun render-file (fdata path root extras)
  "Render a file struct as an html page"
  (let* ((title (ast::file-title fdata)) (f-body (ast::file-body fdata)))
    (htmx::body
     title
     (components::sidebar path root title)
     (:article
      :class "wikipage"
      (when title (spinneret::with-html (:h1 :class "title-top" title)))
      (render-nodelist f-body))
     ;; we have to delay evaluation of the template here, apparently?
     (mapcar (lambda (a) (and a (funcall a))) extras))))
