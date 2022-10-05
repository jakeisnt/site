(load "~/quicklisp/setup.lisp")

(defpackage util
  (:use :cl))

(in-package :util)

(defun write-file (path contents)
  (with-open-file (str path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format str contents)))
