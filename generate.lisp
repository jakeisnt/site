(load "~/quicklisp/setup.lisp")
(load "~/site/src/util.lisp")
(load "~/site/src/index.lisp")
(load "~/site/src/path.lisp")
(load "~/site/src/git.lisp")
(load "~/site/src/components.lisp")

(load "~/site/src/org/org.lisp")
(load "~/site/src/act/act.lisp")

(defparameter *site-location*  "/home/jake/site/docs/")
(defparameter *site-url* "/home/jake/site/docs/")
(defparameter *wiki-location*  "/home/jake/wiki/")

;; pattern matching
(ql:quickload :trivia)

(defparameter *homepage-path* (concatenate 'string *wiki-location* "/index.org"))

(defun get-dir-files (dirname)
  "Get all files as an iterator from the source directory."
  (directory (concatenate 'string *wiki-location* dirname "/*.*")))

(defun parse-file (fpath)
  "Parse a file to its ast representation."
  (with-open-file (stream fpath :direction :input)
    (let ((pathtype (pathname-type fpath)))
      (cond
       ((string= "org" pathtype) (org::parse stream))
       ((string= "act" pathtype) (act::parse stream))
       (t (error "Tried to parse a file type we don't support."))))))

(defun render-html (fdata target-path site-location extras dir-ls)
  "Render an AST to an HTML representation"
  (cond
    ((org::p fdata) (org::html fdata target-path site-location extras dir-ls))
    ((act::p fdata) (act::html fdata target-path site-location extras))
    (t (error "The file type provided doesn't yet have rendering support."))))

(defun generate-file (file-path extras)
  "Generate an isolated html file given a path to a org-mode source."
  (let ((result-path (fpath::change-file-path file-path *wiki-location* *site-location*)))
    (util::write-file
     result-path
     (render-html (parse-file file-path) result-path *site-location* extras '()))))

(defun parse-dir-files (dir-files)
  "parse directory files into an association list with their path"
  (loop for file-path in dir-files
        collect
        (let ((parsed-file (parse-file file-path)))
          (list
           file-path
           (fpath::change-file-path file-path *wiki-location* *site-location*)
           parsed-file
           (git::log-of-file file-path)))))

(defun write-dir-files (dir-ls)
  "Write the directory association list to html files"
  (loop for (src-path target-path fdata git-info) in dir-ls
        do (util::write-file
            target-path
            (render-html
             fdata
             target-path
             *site-location*
             (list
              (lambda ()
                (components::git-history
                 git-info
                 (namestring (fpath::remove-root src-path *wiki-location*)))))
             dir-ls))))

(defun compare-file-dates (a b)
  "Compare two file objects by date."
  (labels ((last-commit-date (c) (caddr (car (cadddr c)))))
    (let ((a-last-time (last-commit-date a))
          (b-last-time (last-commit-date b)))
      (string< b-last-time a-last-time))))

(defun get-dir-dest (dirname)
  "Get the destination directory path and index from the directory name."
  (merge-pathnames (concatenate 'string *wiki-location* dirname "/index.html")))

(defun generate-index (dirname paths)
  "Generate an html file given a path to a org-mode source."
  (let* ((dir-dest (get-dir-dest dirname))
         (result-path (fpath::change-file-path dir-dest *wiki-location* *site-location*)))
    (util::write-file
     result-path
     (html-index::index-page dirname paths *site-location*))))

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

(defun generate-homepage ()
  "Generate the homepage."
  (generate-file
   (merge-pathnames *homepage-path*)
   (list
    #'components::js-disabled
    #'components::lastfm-now-playing
    ;; #'components::last-arena-blocks TODO: Not formatting on the front page as I'd like to
    #'components::link-info
    #'components::lastmod-calendar)))

;; TODO: generate RSS feed from website
;; how to add feed discovery: https://www.petefreitag.com/item/384.cfm

(defun generate ()
  ;; TODO: A general specification for the folder system.
  (let ((spec
          `("pages"                     ; render all files in `pages` folder
            "scripts"
            "index.org"                 ; render this specific file
            ("etc" "script.act"))))     ; render this file in this dir

  ;; BUG: The file must be committed to git to generate a page (atm). Throws weird error otherwise.
  (generate-homepage)
  (generate-dir "pages")
  (generate-dir "scripts")))

;; (generate)
