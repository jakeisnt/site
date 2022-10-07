(load "~/quicklisp/setup.lisp")
(load "./src/util.lisp")
(load "./src/org/html.lisp")
(load "./src/css.lisp")
(load "./src/path.lisp")

(ql:quickload :spinneret)

(defparameter *site-location*  "/home/jake/site/docs")
(defparameter *wiki-location*  "/home/jake/wiki")

(defun change-prefix (str old new)
  "Change the prefix of the string from `old` to `new`"
  (concatenate 'string new (util::without-prefix str old)))

(defun change-postfix (str old new)
  "Change the postfix of the string from `old` to `new`."
  (concatenate 'string (util::without-postfix str old) new))

(defun generate-file (file-path)
  "Generate an html file given a path to an org-mode source."
  (let ((path (fp::make-pathlist file-path "html"))
        (result-path (fp::change-file-path file-path "/p/" *site-location*)))
    (util::write-file
     result-path
     (org-html::render-org-file file-path path *site-location*))))

(defun generate-homepage ()
  (generate-file (concatenate 'string *wiki-location* "/index.org")))

(defun generate-wiki ()
  (loop for file-path in (directory (concatenate 'string *wiki-location* "/pages/**/*.org"))
        do (generate-file file-path)))

(defun generate ()
  (generate-homepage)
  (generate-wiki))

;; (generate)
