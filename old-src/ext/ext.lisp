(load "~/quicklisp/setup.lisp")

(defpackage ext
  (:use :cl))

;; An external link, maybe with a title
(defstruct ext title link)

(defun parse (fstream)
  ;; TODO
  ;; if there is one line, the line is the link
  ;; if there are two lines, the first is the title and the second is the link
  )

(defun html (fdata path root extras)
  ;; TODO
  ()
  )

(defun p (data)
  "Is the file an `ext`?"
  (ext-p data))

(defun filename (data)
  "What's the name of this file?"
  (or (ext-title data) (ext-link data)))
