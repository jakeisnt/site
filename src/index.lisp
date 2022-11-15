(load "~/quicklisp/setup.lisp")
(load "~/site/src/util.lisp")
(load "~/site/src/components.lisp")

(load "~/site/src/act/act.lisp")
(load "~/site/src/org/org.lisp")
(load "~/site/src/org/html.lisp")

(ql:quickload :spinneret)
(ql:quickload 'css-lite)

(defpackage html-index
  (:use :cl))

(in-package html-index)

(defun get-filename (fdata target-path)
  "Get the file name from file data."
  (or
   (cond
     ((act::p fdata) (act::filename fdata))
     ((org::p fdata) (org::filename fdata))
     (t nil))
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
    (htmx::body
     title
     (components::sidebar path root nil)
     (spinneret::with-html
       (:div :class "folder-index-page-table"
             (:table (loop for file-data in flist
                           collect (apply #'index-page-entry (cons root file-data))))))
     nil)))
