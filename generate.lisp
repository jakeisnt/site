(load "~/quicklisp/setup.lisp")
(load "./src/util.lisp")
(load "./src/org/html.lisp")
(load "./src/css.lisp")
(load "./src/path.lisp")

(ql:quickload :spinneret)

(defparameter *site-location*  "/home/jake/site/docs/")
(defparameter *wiki-location*  "/home/jake/wiki/")

(defun change-prefix (str old new)
  "Change the prefix of the string from `old` to `new`"
  (concatenate 'string new (util::without-prefix str old)))

(defun change-postfix (str old new)
  "Change the postfix of the string from `old` to `new`."
  (concatenate 'string (util::without-postfix str old) new))

(defun generate-file (file-path)
  "Generate an html file given a path to a org-mode source."
  (let ((result-path (fp::change-file-path file-path *wiki-location* *site-location*)))
    (util::write-file
     result-path
     (org-html::render-org-file file-path result-path *site-location*))))

(defun generate-homepage ()
  "Generate the homepage."
  (generate-file (merge-pathnames (concatenate 'string *wiki-location* "/index.org"))))

(defun index-page (urls)
  (htmlgen::body
   *site-location*
   (concatenate 'string "page" "/index")
   (loop for url in urls
         collect (:a :href url "name for url"))))

;; page looks like this:
;;
;; acl2 .............. html
;; 100_rabbits ........ org
;; name .............. type
;;
;; and every one of these is a clickable link to the article

;; we also need to create an index page here for each
(defun generate-wiki ()
  (loop for file-path in (directory (concatenate 'string *wiki-location* "/pages/**/*.org"))
        do (generate-file file-path)))

(defun generate ()
  (generate-homepage)
  (generate-wiki))

;; (generate)
