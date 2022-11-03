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


(defmacro body (title out-of-main contents extra)
  "The body of every HTML page."
  `(spinneret::with-html-string
       (:html
        :lang "en-us"
        (:head
         (:meta :charset "utf-8")
         (:title (concatenate 'string (when ,title ,title) (when ,title " | ") "Jake Chvatal"))

         (:meta :name "viewport" :content "width=device-width,initial-scale=1.0")
         ;; The Open Graph Protocol. More info here: https://ogp.me/
         (:meta :property "og:title" :content (or ,title "Jake Chvatal"))
         (:meta :property "og:type" :content "website")
         (:meta :property "og:url" :content "https://jake.isnt.online")
         (:meta :property "og:image" :content "https://avatars0.githubusercontent.com/u/29869612?s=400&u=32e0c272cbfcc32b8e9585f74ce57d197aa14fb0&v=4")
         (:meta :property "og:site_name" :content "Jake Chvatal")
         (:meta :name "description" :content "Hi")
         (:meta :name "keywords"
                :content (concatenate 'string
                                      (when ,title (concatenate 'string ,title ", "))
                                      "webring, programming, languages")) ;; TODO: add cool words from contents to meta tag?
         (:meta :name "author" :content "Jake Chvatal")
         (:meta :name "robots" :content "follow")
         (:meta :name "theme-color" :content "#fff")

         (:link :rel "icon" :type "image/x-icon" :href "/favicon.ico")
         (:link :rel "apple-touch-icon" :type "image/png" :sizes "180x180" :href "/apple-touch-icon.png")
         (:link :rel "icon" :type "image/png" :sizes "32x32" :href "/favicon-32x32.png")
         (:link :rel "icon" :type "image/png" :sizes "16x16" :href "/favicon-16x16.png")
         (:link :rel "manifest" :href "/site.webmanifest") ;; TODO: manifest is currently mostly a lie
         (:link :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/styles/default.min.css")
         (:link :rel "stylesheet" :href "/style.css")

         (:script :src "/lib.js")
         ;; highlight js
         (:script :src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/highlight.min.js")
         (:script "hljs.highlightAll();"))
        ;; mathjax: TODO: Not working
        ;; (:script :src "https://polyfill.io/v3/polyfill.min.js?features=es6")
        ;; (:script :id "MathJax-script" :src "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
        ;;  "
        ;;   MathJax.typesetPromise().then(() => {
        ;;   // modify the DOM here
        ;;   MathJax.typesetPromise();
        ;;   }).catch((err) => console.log(err.message));
        ;;  ")
        (:body
         (:div :class "site-body"
               :id "site-body"
               ,out-of-main
               (:main ,contents)
               ,extra
               ;; (components::checkbox-menu)
               )))))

;; TODO: three cases:
;; `id:`: this should find the file with the given id and link to it.
;; `file:`: this should get the file at the path and link to it.
;;          this could, unfortunately, be a relative path.
;; `https://`: leave it alone. this link works.
;; There may be other cases. Handle those!
;; I also might want to allow for opening internal links
;;   as separate panes on the same page rather than as new pages,
;;   but only on the desktop. How do I do this?
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

(defun section-title (header)
  (let ((title (ast::header-title header)))
    ;; replace  spaces with

    )
  )

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
     (:p (ast::quote-block-body node))
     (:cite (render-nodelist (ast::quote-block-author node))))))

;; macro: https://github.com/ruricolist/spinneret
;; TODO: add edit icon.  this can just take to github page,
;; or open up buffer and take to github on save,
;; or something else - maybe my own endpoint? just has to be able to give user ability to contribute.
(defun render-file (fdata path root extras)
  "Render a file struct as an html page"
  (let* ((title (ast::file-title fdata)) (f-body (ast::file-body fdata)))
    (body
     title
     (components::sidebar path root title)
     (:article
      :class "wikipage"
      (when title (spinneret::with-html (:h1 :class "title-top" title)))
      (render-nodelist f-body))
     ;; we have to delay evaluation of the template here, apparently?
     (mapcar (lambda (a) (and a (funcall a))) extras))))


(defun get-filename (fdata target-path)
  "Get the file name from file data."
  (or
   (ast::file-title fdata)
   (pathname-name target-path)))

;; we also need to create an index page here for each
(defun index-page (dirname flist root)
  "Generate an index page from a list of paths at that index and a directory name."

  (let ((path (path::rename (cadr (car flist)) "index"))
        (title (concatenate 'string dirname "/index")))
    (body
     title
     (components::sidebar path root nil)
     (spinneret::with-html
       (:div :class "folder-index-page-table"
             (:table (loop for (src-path target-path fdata git-hist) in flist
                           collect
                           (let ((name (get-filename fdata target-path))
                                 (last-updated (car git-hist)))
                             (spinneret::with-html
                               (:tr
                                (:td (caddr last-updated))
                                (:td :class "file-name-tr"
                                     (:a
                                      :id (concatenate 'string "indexmenu-" name)
                                      :href (path::remove-root target-path root)
                                      name))
                                (:td (car last-updated)))))))))
     (list))))
