
(load "~/quicklisp/setup.lisp")

(defpackage defs
  (:use :cl))

(in-package defs)

;; a link
(defstruct link title url)
