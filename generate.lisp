(load "~/quicklisp/setup.lisp")
(load "~/site/src/util.lisp")
(load "~/site/src/html.lisp")
(load "~/site/src/path.lisp")
(load "~/site/src/git.lisp")
(load "~/site/src/components.lisp")
(load "~/site/src/org/parser.lisp")

(defparameter *site-location*  "/home/jake/site/docs/")
(defparameter *site-url* "/home/jake/site/docs/")
(defparameter *wiki-location*  "/home/jake/wiki/")

(defparameter *homepage-path* (concatenate 'string *wiki-location* "/index.org"))

(defun generate-file (file-path extras)
  "Generate an html file given a path to a org-mode source."
  (let ((result-path (fpath::change-file-path file-path *wiki-location* *site-location*)))
    (util::write-file
     result-path
     (htmx::render-file (parser::parse file-path) result-path *site-location* extras))))

(defun generate-homepage ()
  "Generate the homepage."
  (generate-file
   (merge-pathnames *homepage-path*)
   (list
    #'components::js-disabled
    #'components::lastfm-now-playing
    #'components::last-arena-blocks
    #'components::link-info)))


(defun get-dir-dest (dirname)
  "Get the destination directory path and index from the directory name."
  (merge-pathnames (concatenate 'string *wiki-location* dirname "/index.html")))


(defun generate-index (dirname paths)
  "Generate an html file given a path to a org-mode source."
  (let* ((dir-dest (get-dir-dest dirname))
         (result-path (fpath::change-file-path dir-dest *wiki-location* *site-location*)))
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
                 (fpath::change-file-path file-path *wiki-location* *site-location*)
                 (parser::parse file-path)
                 (git::log-of-file file-path))))


(defun write-dir-files (dir-ls)
  "Write the directory association list to html files"
  (loop for (src-path target-path fdata git-info) in dir-ls
        do (util::write-file
            target-path
            (htmx::render-file
             fdata
             target-path
             *site-location*
             (list
              (lambda ()
                (components::git-history
                 git-info
                 (namestring (fpath::remove-root src-path *wiki-location*)))))))))


(defun compare-file-dates (a b)
  "Compare two file objects by date."
  (labels ((last-commit-date (c) (caddr (car (cadddr c)))))
    (let ((a-last-time (last-commit-date a))
          (b-last-time (last-commit-date b)))
      (string< b-last-time a-last-time))))

(defun generate-dir (dirname)
  "
   Generate a series of HTML files from a directory.
   The `dirname` provided is relative to the homepage of the wiki.
  "
  (let* ((dir-files (get-dir-files dirname))
         (parsed-files (parse-dir-files dir-files))
         (sorted-files (sort (copy-seq parsed-files) #'compare-file-dates)))

    (write-dir-files sorted-files)
    (generate-index dirname sorted-files)))


;; TODO: generate RSS feed from website
;; how to add feed discovery: https://www.petefreitag.com/item/384.cfm

(defun generate-from-spec (folder)
  (loop for form in folder
        do (cond
             ((stringp form) (generate-file)))))

(defun generate ()
  ;; A specification for the folder system.
  (let ((spec
          `(("pages")                   ; render all files in `pages` folder
            "index.org"                 ; render this specific file
            ("etc" "script.act"))))     ; render this file in this dir

    (generate-homepage)
    (generate-dir "pages")))

;; (generate)
