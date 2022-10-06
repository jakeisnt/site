(load "~/quicklisp/setup.lisp")

(defpackage defs
  (:use :cl))

(in-package defs)

;; a link ([[link][text]])
(defstruct link title url)

;; bold text (*text*)
(defstruct bold text)

;; italicised text (/text/)
(defstruct ital text)

;; verbatim text (`text`)
(defstruct verb text)
