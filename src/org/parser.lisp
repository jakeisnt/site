(load "~/quicklisp/setup.lisp")
(load "~/site/src/org/ast.lisp")
(load "~/site/src/org/lexer.lisp")

(ql:quickload :string-case)
(ql:quickload :cl-ppcre)

(defpackage parser
  (:use :cl :string-case :cl-ppcre))

(in-package parser)

(defun parse (fname)
  "parse an org-mode file to an internal, struct-based representation"
  (parse-all (lexer::tokenize stream)))

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
  (or (not (ast::header-p elem))
      (> (ast::header-rank elem) cur-header-rank)))

(defun split-header (cur-header cur-doc)
  "Split the current document based on the current header;
   place everything in the cur-doc below the rank of the header
   in the header's body.
  "
  (let
      ((cur-header-title (lexer::header-tok-title cur-header))
       (cur-header-rank (lexer::header-tok-rank cur-header)))
    (cons
     (ast::make-header
      :rank cur-header-rank
      :title cur-header-title
      :body (take-while
             (lambda (elem) (is-below-headerp cur-header-rank elem))
             cur-doc))
     (drop-while
      (lambda (elem) (is-below-headerp cur-header-rank elem))
      cur-doc))))

;; NOTE: mega hack (probably)
;; Yeah, this produced a bug.
;; We've fixed it, but the better solution is to create
;; a local function that closes over a locally scoped variable.
(defvar file-name nil)

;; Add a bullet to a bullet group
(defun add-bullet (bullet bullet-group)
  (ast::make-bullet-group :body (cons bullet (ast::bullet-group-body bullet-group))))

(defun start-bullet-group (bullet)
  (ast::make-bullet-group :body (list bullet)))

(defun bullet-tok->bullet (cur-tok)
  (ast::make-bullet :body (lexer::bullet-tok-body cur-tok)))

(defun text-tok->text (cur-tok)
  (ast::make-text :body (lexer::text-tok-body cur-tok)))

(defun fuse-bullet (bullet-tok acc)
  (let ((last-tok (car acc))
        (bullet (bullet-tok->bullet bullet-tok)))
    (if (ast::bullet-group-p last-tok)
        (cons (add-bullet bullet last-tok) (cdr acc))
        (cons (start-bullet-group bullet) acc))))

;; (defun parse-quote-block (cur-tok acc)
;;   (cons
;;    (ast::make-quote-block
;;     :body (parse-tokens (ast::quote-block-body cur-tok) nil))
;;    acc))

(defun parse-tokens (token-list acc)
  "Parse tokens into a recursive structure from a token list."
  (let ((cur-tok (car token-list)))
    (if (not cur-tok)
        (let ((res (ast::make-file :title file-name :body acc)))
          (setq file-name nil)
          res)
        (parse-tokens
         (cdr token-list)
         (cond
           ((lexer::header-tok-p cur-tok) (split-header cur-tok acc))
           ((lexer::text-tok-p cur-tok) (cons (text-tok->text cur-tok) acc))
           ((lexer::title-tok-p cur-tok)
            (setq file-name (lexer::title-tok-title cur-tok))
            acc)
           ((lexer::bullet-tok-p cur-tok) (fuse-bullet cur-tok acc))
           ;; ((ast::quote-block-p cur-tok) (parse-quote-block cur-tok acc))
           (t (cons cur-tok acc)))))))

(defun parse-all (tks)
  "Parse a list of tokens"
  (parse-tokens (reverse tks) ()))
