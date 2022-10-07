
(load "~/quicklisp/setup.lisp")
(load "./src/util.lisp")

(ql:quickload :spinneret)
(ql:quickload 'css-lite)
(ql:quickload :parenscript)

(defpackage comp
  (:use :cl))

(in-package :comp)

(defun checkbox-menu ()
  (spinneret::with-html
    (:div :class "checkbox-menu"
          (:input
           :type "checkbox"
           :id "hypothesis-checkbox"
           :checked "false"
           :onclick (parenscript:ps (toggle-hypothesis))
           "hypothes.is"))))

;; () -> (:a :href jake/indexx)
;; ("filename") -> (:a :href jake/filename)
;; ("p" "filename") -> (:a :href jake/p/index) (:a :href jake/p/filename)
;; ("a" "p" "filename")  -> (:a :href jake/a/index) (:a :href jake/a/p/index) (:a :href jake/a/p/filename)

;; i want this site to feel like the navigation of a file system;
;; view version control, depth, what's in current dir;
;; support files of all types;
;; ideally this just doubles as a source code viewer, honestly
;; should mousing over reveal alternative things for this path? is that right?

;; can mouse over to view three options; can click '...' (or scroll?) to view more?


;; TODO before I merge this PR:
;; - finish this pathlist.
;; - make the homepage into an org-mode file.
;; - generate an indexx file for the /p folder.

(defmacro make-pathlist (ls)
  `(loop for pathpart in ls
         collect (:a :href (concatenate 'string root "/" pathpart) )
         )
  )

;; TODO this needs to be done still!
(defun sidebar (pathlist)
  (let ((root "https://jake.isnt.online"))
    (spinneret::with-html
        (:div
         :class "sidebar"
         (:a :href root "~ ")
         (make-pathlist pathlist)

         ))))
