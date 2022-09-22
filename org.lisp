(load "~/quicklisp/setup.lisp")

(ql:quickload :cl-org-mode)
(ql:quickload :trivia)

(defpackage org-test
  (:use :cl :cl-org-mode :trivia))

(in-package org-test)

(defparameter *file*
  (cl-org-mode::read-org-file
   (pathname "./README.org")))

(setf *file* (cl-org-mode::node.next-node *file*))

*file*

(cl-org-mode::node.text *file*)
(cl-org-mode::node.heading *file*)
(cl-org-mode::node.children *file*)
(cl-org-mode::node.pathname *file*)

(cl-org-mode::node.text (cl-org-mode::node.next-node *file*))

(defun print-org-file (file-path)
  (let ((org-file (cl-org-mode::node.next-node (cl-org-mode::read-org-file file-path))))
    (match org-file
      ((type cl-org-mode::outline-node) (cl-org-mode::node.heading org-file))
      ((type cl-org-mode::text-node) (cl-org-mode::node.text org-file))
      ((type cl-org-mode::src-node) (cl-org-mode::node.text org-file))
      (otherwise "pee"))))

(print-org-file (pathname "/home/jake/site/README.org"))
