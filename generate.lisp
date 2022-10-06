(load "~/quicklisp/setup.lisp")
(load "./src/util.lisp")
(load "./src/homepage.lisp")
(load "./src/org/html.lisp")
(load "./src/css.lisp")

(ql:quickload :spinneret)

(defparameter *site-location*  "~/site/docs/")
(defparameter *wiki-location*  "~/wiki/")
;; (defparameter org-html::*url* "https://jake.isnt.online")

(defun string-append (str1 str2)
  (concatenate 'string str1 str2))

(defun generate-homepage ()
  "Generate the homepage!"
  (util::write-file
   (string-append *site-location* "index.html")
   (spinneret::with-html-string (homepage::homepage))))

(defun change-prefix (str old new)
  "Change the prefix of the string from `old` to `new`"
  (string-append new (util::without-prefix str old)))

(defun change-postfix (str old new)
  "Change the postfix of the string from `old` to `new`."
  (string-append (util::without-postfix str old) new))

(defun change-file-path (current-path)
  "Change a file path to a new name."
  (concatenate
     'string
     "/home/jake/site/docs/p/"
     (pathname-name current-path)
     ".html"))

(defun generate-wiki ()
  (loop for file-path in (directory "~/wiki/pages/**/*.org")
        do (util::write-file
            (change-file-path file-path)
            (org-html::render-org-file file-path))))

;; (defun generate-global-css ()
;;   (util::write-file "/home/jake/site/docs/style.css" (css::style)))

(defun generate ()
  (generate-homepage)
  (generate-wiki)
  ;; (generate-global-css)
  )
