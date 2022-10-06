(load "~/quicklisp/setup.lisp")
(load "./src/org/lexer.lisp")

(ql:quickload :string-case)
(ql:quickload :cl-ppcre)

(defpackage parser
  (:use :cl :string-case :cl-ppcre))

(in-package parser)

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

(defun parse (fname)
  "parse an org-mode file to an internal, struct-based representation"
  (with-open-file (stream fname :direction :input :if-does-not-exist nil)
    (if stream (parse-all (lexer::tokenize stream)) 'no-stream)))

(defun parse-all (tks)
  "Parse a list of tokens"
  (parse-tokens (reverse tks) ()))

(defun take-while (pred list)
  "Keep top elements from the list that fail the predicate"
  (loop for x in list
        while (funcall pred x)
        collect x))

(defun drop-while (pred list)
  "Drop top elements from the list that fail the predicate"
  (let ((to-rm (take-while pred list)))
    (subseq list (length to-rm))))

(defun is-below-headerp (cur-header-rank elem)
  "Does the current element rank below the current header rank?"
  (or (not (header-p elem))
      (> (header-rank elem) cur-header-rank)))

(defun split-header (cur-header cur-doc)
  "Split the current document based on the current header;
   place everything in the cur-doc below the rank of the header
   in the header's body.
  "
  (let
      ((cur-header-title (lexer::header-tok-title cur-header))
       (cur-header-rank (lexer::header-tok-rank cur-header)))
    (cons
     (make-header
      :rank cur-header-rank
      :title cur-header-title
      :body (take-while
             (lambda (elem) (is-below-headerp cur-header-rank elem))
             cur-doc))
     (drop-while
      (lambda (elem) (is-below-headerp cur-header-rank elem))
      cur-doc))))

;; NOTE: mega hack (probably)
(defvar file-name nil)

(defun parse-tokens (token-list acc)
  "Parse tokens into a recursive structure from a token list."
  (let ((cur-tok (car token-list)))
    (if (not cur-tok)
        (make-file :title file-name :body acc)
        (parse-tokens
         (cdr token-list)
         (cond
           ((lexer::header-tok-p cur-tok) (split-header cur-tok acc))
           ((lexer::text-tok-p cur-tok)
            (cons (make-text :body (lexer::text-tok-body cur-tok)) acc))
           ((lexer::title-tok-p cur-tok)
            (setq file-name (lexer::title-tok-title cur-tok))
            acc)
           ((lexer::code-block-tok-p cur-tok)
            (cons (make-code-block
                   :lang (lexer::code-block-tok-lang cur-tok)
                   :body (lexer::code-block-tok-body cur-tok)) acc))
           ((lexer::bullet-tok-p cur-tok)
            (cons (make-bullet :body (lexer::bullet-tok-body cur-tok)) acc))
           (t (cons cur-tok acc)))))))
