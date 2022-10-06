(load "~/quicklisp/setup.lisp")

(defpackage util
  (:use :cl))

(in-package util)

(defun write-file (path contents)
  (with-open-file (str path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format str "~S" contents)))

(defun string-postfixesp (larger smaller)
  "Determine whether the assumed smaller string postfixes the larger."
  (let ((sm-len (length smaller))
        (lg-len (length larger)))
    (and
     (>= lg-len sm-len)
     (string= smaller larger :start2 (- lg-len sm-len) :end2 lg-len))))

(defun string-prefixesp (larger smaller)
  "Determine whether the assumed smaller string postfixes the larger."
  (let ((sm-len (length smaller))
        (lg-len (length larger)))
    (and
     (>= lg-len sm-len)
     (string= smaller larger :start2 0 :end2 sm-len))))

(defun without-postfix (larger smaller)
  "Produce the larger string without its assumed postfix, smaller"
  (subseq larger 0 (- (length larger) (length smaller))))

(defun without-prefix (larger smaller)
  "Produce the larger string without its assumed prefix, smaller"
  (subseq larger (length smaller) (length larger)))
