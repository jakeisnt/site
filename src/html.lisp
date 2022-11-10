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
  `(concatenate
    'string
    "<!DOCTYPE html>"
    (spinneret::with-html-string
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
              ))))))


(defun get-filename (fdata target-path)
  "Get the file name from file data."
  (or
   (ast::file-title fdata)
   (pathname-name target-path)))

(defun commit-date (commit-struct)
  "Get commit date from commit tuple"
  (caddr commit-struct))

(defun index-page-entry (root src-path target-path fdata git-hist)
  "Generate an index page entry for a particular file."
  (let ((name (get-filename fdata target-path))
        (last-updated (car git-hist))
        (created (car (last git-hist))))
    (spinneret::with-html
      (:tr
       (:td (car last-updated))
       (:td :class "file-name-tr"
            (:a
             :id (concatenate 'string "indexmenu-" name)
             :href (fpath::remove-root target-path root)
             name))
       (:td (commit-date last-updated))))))

;; we also need to create an index page here for each
(defun index-page (dirname flist root)
  "Generate an index page from a list of paths at that index and a directory name."
  (let ((path (fpath::rename (cadr (car flist)) "index"))
        (title (concatenate 'string dirname "/index")))
    (body
     title
     (components::sidebar path root nil)
     (spinneret::with-html
       (:div :class "folder-index-page-table"
             (:table (loop for file-data in flist
                           collect (apply #'index-page-entry (cons root file-data))))))
     nil)))
