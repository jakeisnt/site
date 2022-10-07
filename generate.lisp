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


(defun index-page (urls)
  (htmlgen::body
   (loop for url in urls
         collect (:a :href url name))))

;; page looks like this:
;;
;; acl2 .............. html
;; 100_rabbits ........ org
;; name .............. type
;;
;; and every one of these is a clickable link to the article

(defun generate-wiki ()
  (loop for file-path in (directory (concatenate 'string *wiki-location* "/pages/**/*.org"))
        do (generate-file file-path))

  ;; we also need to create some index here

  )

(defun generate ()
  (generate-homepage)
  (generate-wiki))

;; (generate)
