(load "~/quicklisp/setup.lisp")

(defpackage ast
  (:use :cl))

(in-package ast)

;; a link ([[link][text]])
(defstruct link title url)

;; bold text (*text*)
(defstruct bold text)

;; italicised text (/text/)
(defstruct ital text)

;; verbatim text (`text`)
(defstruct verb text)

;; The contents of an org-mode file
(defstruct file title metadata body)

;; A heading with its corresponding body
(defstruct header title body rank)

;; a passage of plain text
(defstruct text body)

;; a bullet point
(defstruct bullet body)

;; a code block
(defstruct code-block lang body)
