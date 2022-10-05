
(load "~/quicklisp/setup.lisp")

(defpackage defs
  (:use :cl))

;; a link
(defstruct link title url)
