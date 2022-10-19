(load "~/quicklisp/setup.lisp")
(load "~/site/src/util.lisp")

(ql:quickload :string-case)
(ql:quickload :cl-ppcre)

(defpackage parse-help
  (:use :cl :string-case :cl-ppcre))

(in-package :parse-help)

(defun take-until-char (stream end-on)
  "take from a stream until a particular character is received
   taken from another file"
  (let ((chars (make-adjustable-string "")))
    (loop for next-char = (safe-read-char stream)
          until (or (eq next-char end-on)
                    (eq next-char :eof)
                    (and (stringp end-on)
                         (util::string-postfixesp chars end-on)))
          do (push-char chars next-char))
    chars))

(defun make-adjustable-string (s)
  "Make a string that's easily appendable to."
  (make-array (length s)
              :fill-pointer (length s)
              :adjustable t
              :initial-contents s
              :element-type (array-element-type s)))


(defun push-char (str c)
  "Push a character to an adjustable string."
  (vector-push-extend c str))

(defun safe-read-char (stream)
  "Safely read a character from a stream"
  (read-char stream nil :eof))

(defun take-until-char (stream end-on)
  "take from a stream until a particular character is received"
  (let ((chars (make-adjustable-string ""))
        (hit-eof nil))
    (loop for next-char = (safe-read-char stream)
          until (or (eq next-char end-on)
                    (and (eq next-char :eof)
                         (setq hit-eof t))
                    (and (stringp end-on)
                         (util::string-postfixesp chars end-on)))
          do (push-char chars next-char))
    (if hit-eof :eof chars)))

(defun safe-read-line (stream)
  "Safely read a line from a stream, producing a string."
  (take-until-char stream #\newline))

(defun starts-with (chr line)
  "Does the provided line start with the provided character?"
  (and (< 0 (length line)) (eq (char line 0) chr)))

(defun can-split-on (chr line)
  "Can the line split on the provided character?"
  (> 1 (length (split (string chr) line :limit 2))))

(defun is-eof (line)
  "Is the line an eof?"
  (eq line :eof))
