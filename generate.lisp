(load "~/quicklisp/setup.lisp")
(load "~/site/src/util.lisp")
(load "~/site/src/html.lisp")
(load "~/site/src/path.lisp")
(load "~/site/src/git.lisp")
(load "~/site/src/org/parser.lisp")

(defparameter *site-location*  "/home/jake/site/docs/")
(defparameter *site-url* "/home/jake/site/docs/")
(defparameter *wiki-location*  "/home/jake/wiki/")

(defun generate-file (file-path)
  "Generate an html file given a path to a org-mode source."
  (let ((result-path (path::change-file-path file-path *wiki-location* *site-location*)))
    (util::write-file
     result-path
     (htmx::render-file (parser::parse file-path) result-path *site-location*))))

(defun generate-homepage ()
  "Generate the homepage."
  (generate-file (merge-pathnames (concatenate 'string *wiki-location* "/index.org"))))


(defun get-dir-dest (dirname)
  "Get the destination directory path and index from the directory name."
  (merge-pathnames (concatenate 'string *wiki-location* dirname "/index.html")))


(defun generate-index (dirname paths)
  "Generate an html file given a path to a org-mode source."
  (let* ((dir-dest (get-dir-dest dirname))
         (result-path (path::change-file-path dir-dest *wiki-location* *site-location*)))
    (util::write-file
     result-path
     (htmx::index-page dirname paths *site-location*))))



(defun get-dir-files (dirname)
  "Get all org-mode files as an iterator from the source directory."
  (directory (concatenate 'string *wiki-location* dirname "/*.org")))


(defun parse-dir-files (dir-files)
  "parse directory files into an association list with their path"
  (loop for file-path in dir-files
        collect (list
                 file-path
                 (path::change-file-path file-path *wiki-location* *site-location*)
                 (parser::parse file-path)
                 (git::log-of-file file-path))))


(defun write-dir-files (dir-ls)
  "Write the directory association list to html files"
  (loop for (src-path target-path fdata) in dir-ls
        do (util::write-file
              target-path
              (htmx::render-file fdata target-path *site-location*))))


(defun generate-dir (dirname)
  "
   Generate a series of HTML files from a directory.
   The `dirname` provided is relative to the homepage of the wiki.
  "
  (let* ((dir-files (get-dir-files dirname))
         (parsed-files (parse-dir-files dir-files)))

    (write-dir-files parsed-files)
    (generate-index dirname parsed-files)))

(defun generate ()
  (generate-homepage)
  (generate-dir "pages"))

;; (generate)
