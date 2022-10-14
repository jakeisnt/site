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


(defun generate-index (dirname dir-dest paths)
  "Generate an html file given a path to a org-mode source."
  (let ((result-path (fp::change-file-path dir-dest *wiki-location* *site-location*)))
    (util::write-file
     result-path
     (spinneret::with-html-string (index-page dirname paths)))))

;; page looks like this:
;;
;; acl2 .............. html
;; 100_rabbits ........ org
;; name .............. type
;;
;; and every one of these is a clickable link to the article

;; we also need to create an index page here for each
(defun generate-dir (dirname)

  (let* ((dir-dest (merge-pathnames (concatenate 'string *wiki-location* dirname "/index.html")))
         (dir-files (directory (concatenate 'string *wiki-location* dirname "/*.org"))))
    (loop for file-path in dir-files
          do (generate-file file-path))

    (generate-index dirname dir-dest dir-files)))

(defun generate ()
  (generate-homepage)
  (generate-dir "pages"))

;; (generate)
