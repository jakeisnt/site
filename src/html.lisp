
(ql:quickload :spinneret)
(ql:quickload 'css-lite)
(ql:quickload :parenscript)

(defpackage htmlgen
  (:use :cl))

(in-package htmlgen)

(defun body (root title contents)
  (spinneret::with-html
     (:html
      :lang "en-us"
      (:head
       (:title (concatenate 'string (or title "") " | Jake Chvatal"))
       (:link :rel "stylesheet" :href (concatenate 'string root "/style.css"))
       (:link :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/styles/default.min.css")
       (:script :src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/highlight.min.js")
       (:script "hljs.highlightAll();"))
      (:body
       (:main contents)))))
