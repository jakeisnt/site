(load "~/quicklisp/setup.lisp")
(load "~/site/src/org/ast.lisp")
(load "~/site/src/util.lisp")
(load "~/site/src/components.lisp")

(ql:quickload :spinneret)
(ql:quickload 'css-lite)
(ql:quickload :parenscript)

(defpackage htmx
  (:use :cl))

(in-package htmx)

(defmacro body (title contents)
  "The body of every HTML page."
  `(spinneret::with-html-string
    (:html
     :lang "en-us"
     (:head
      (:title (concatenate 'string (or ,title "?") " | Jake Chvatal"))
      (:meta :name "viewport" :content "width=device-width,initial-scale=1.0")
      (:link :rel "stylesheet" :href "/style.css")
      (:link :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/styles/default.min.css")
      (:script :src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/highlight.min.js")
      (:script "hljs.highlightAll();"))
     (:body
      (:main ,contents)))))

;; three cases:
;; `id:`: this should find the file with the given id and link to it.
;; `file:`: this should get the file at the path and link to it.
;;          this could, unfortunately, be a relative path.
;; `https://`: leave it alone. this link works.
(defun convert-link (txt)
  "Convert a link to an HTML link."
  (spinneret::with-html
      (let* ((url (ast::link-url txt))
             (title (or (ast::link-title txt) url))
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
      ((ast::link-p txt) (convert-link txt))
      ((ast::bold-p txt) (:b (ast::bold-text txt)))
      ((ast::ital-p txt) (:i (ast::ital-text txt)))
      ((ast::verb-p txt) (:pre txt))
      (t "fell through the cracks"))))

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
    (:ul (:li (render-text-body (ast::bullet-body node))))))

(defun code-block (node)
  (spinneret::with-html
    (:pre
     (:code
      :class (concatenate 'string "language-" (ast::code-block-lang node))
      (ast::code-block-body node)))))

(defun render-node (node)
  "Render a node as HTML."
  (spinneret::with-html
    (cond
      ((ast::header-p node) (header node))
      ((ast::text-p node) (text node))
      ((ast::bullet-p node) (bullet node))
      ((ast::code-block-p node) (code-block node))
      (t nil))))

;; macro: https://github.com/ruricolist/spinneret
(defun render-file (fdata path root)
  "Render a file struct as an html page"
  (let* ((title (ast::file-title fdata)) (f-body (ast::file-body fdata)))
    (body
     title
     (list
      (components::sidebar path root)
      (when title (spinneret::with-html (:h1 :class "title-top" title)))
      (loop for node in f-body
            collect (render-node node))))))


;; page looks like this:
;;
;; acl2 .............. html
;; 100_rabbits ........ org
;; name .............. type
;;
;; and every one of these is a clickable link to the article

;; we also need to create an index page here for each
(defun index-page (dirname flist root)
  "Generate an index page from a list of paths at that index and a directory name."

  (let ((path (path::rename (cadr (car flist)) "index"))
        (title (concatenate 'string dirname "/index")))

    (body
     title
     (list
      (components::sidebar path root)
      (spinneret::with-html
        (:div :class "url-cage"
              (loop for (src-path target-path fdata) in flist
                    collect
                    (let ((name (or
                                 (ast::file-title fdata)
                                 (pathname-name target-path))))
                      (spinneret::with-html
                        (:a :id (concatenate 'string "indexmenu-" name) :href (path::remove-root target-path root) name))))))))))
