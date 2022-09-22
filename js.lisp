(load "~/quicklisp/setup.lisp")

(ql:quickload :parenscript)

(defpackage ps-tutorial
  (:use :parenscript))

(in-package :ps-tutorial)

;; i want common lisp to have some traversable evaluation tree
;; in which i can view previous or future states of the editor before and after
;; evaluation in an immutable fashion. that would be cool.

(ps
  (defun dyn-load (src id)
    (let ((s (chain document (create-element "script"))))
      (chain s (set-attribute "src" src))
      (chain s (set-attribute "id" id))
      s))

  (defun load-hypothesis ()
    (dyn-load "https://hypothes.is/embed.js" "hypothesis"))

  (defun unload-hypothesis ()
    (let ((annotator-link (chain document (query-selector "link[type=\"application/annotator+html\"]"))))
      (if annotator-link
        (let ((destroy-event (new (-Event "destroy"))))
          (chain annotator-link (dispatch-event destroy-event))))))

  (defun greeting-callback ()
    (alert "Hello World"))

  (greeting-callback))
