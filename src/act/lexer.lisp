
;; An AST that represents conversations.
(defpackage act-ast
  (:use :cl))

(in-package act-ast)

;; A name alias, for brevity
;; NOTE: This may not be needed.
;; It might be best to handle during parsing.
;; (defstruct alias nick name)

;; A single message from a person
(defstruct message author text)

;; Some context for the scene.
;; Concretely: (Context.)
;; This can also be inline
(defstruct context text)

;; An italicised section: /ital/.
(defstruct ital text)

;; A name alias in the body. TODO. Some cool ideas here.
(defstruct alias name)


(load "~/quicklisp/setup.lisp")
(load "~/site/src/act/help.lisp")

;; A parser for `.act` files.
(defpackage act-parser
  (:use :cl :cl-ppcre :parse-help))

(in-package act-parser)

(ql:quickload :cl-ppcre)

;; Read a line.
;; If we see a `-`, ignore the line.
;; If we see a `(', assume a closing.
;; If we see some A = B, we collect it as a binding.
;; If we see some A: B, it's a line.
;;   Inside the lines, (parentheses) and /italics/ are valid.

(defun parse-context (line)
  "Parse context as a line."
  (act-ast::make-context :text (subseq line 1 (- (length line) 1))))

(defun parse-message (line)
  "Parse a message as a line."
  (let ((line-parts (split ": " line :limit 2)))
    (act-ast::make-message (car line-parts) (cadr line-parts))))

(defmacro match-line (recur stream cnd)
  "Abstract out the practice of matching on a line and looking for something."
  `(let ((line (parse-help::safe-read-line stream)))
     (print line)
     (if (parse-help::is-eof line)
         nil
         (let ((readline-res ,cnd))
           (print readline-res)
           (if (eq readline-res :ignore)
                (funcall ,recur ,stream)
               (cons readline-res (funcall ,recur ,stream)))))))

(defun parse (stream)
  "Parse a script from a stream, assuming we're at the start of a line."
  (match-line
   #'parse
   stream
   (cond
     ((parse-help::starts-with  #\- line) :ignore)
     ((parse-help::starts-with  #\( line) (parse-context line))
     ((parse-help::can-split-on #\: line) (parse-message line))
     ((parse-help::can-split-on #\= line) :ignore) ;; (parse-alias stream)
     (t
      (print "not matching any case, ignoring")
      :ignore))))
