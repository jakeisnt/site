(load "~/quicklisp/setup.lisp")

(ql:quickload :string-case)

(defpackage parse-org
  (:use :cl :string-case))

(in-package parse-org)





;; what does our file have?
;; file: title, metadata, body
;; body: listof (or heading text code)
;; heading: title, body
;; text: listof 
;; code: language, contents

;; The contents of an org-mode file
(defstruct file title metadata body)

;; A heading with its corresponding body
(defstruct header title body rank)
;; (heading-p (make-header :title "asdf" :body "asdf"))

;; a link with text and a url
;; (defstruct link text url)

(defun parse (fname)
  "parse an org-mode file to an internal, struct-based representation"
  (with-open-file (stream fname :direction :input :if-does-not-exist nil)
    (if stream (parse-all (tokenize stream)) 'no-stream)))

(defun parse-all (tks)
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
  (print "is the following elem below the current header rank?")
  (print cur-header-rank)
  (print elem)
  (or (not (header-p elem))
      (> (header-rank elem) cur-header-rank)))

(defun split-header (cur-header cur-doc)
  "Split the current document based on the current header;
   place everything in the cur-doc below the rank of the header
   in the header's body.
  "
  (print cur-header)
  (let
      ((cur-header-title (header-tok-title cur-header))
       (cur-header-rank (header-tok-rank cur-header)))
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

(defun parse-tokens (token-list acc)
  "Parse tokens into a recursive structure from a token list."
  (let ((cur-tok (car token-list)))
    (if (not cur-tok)
        acc
        (parse-tokens
         (cdr token-list)
         (cond
           ((header-tok-p cur-tok) (split-header cur-tok acc))
           (t (cons cur-tok acc)))))))

;;  --- Tokenize ---

;; A header token with a rank and title
(defstruct header-tok rank title)
;; A text token with a body
(defstruct text-tok body)
;; a document title
(defstruct title-tok title)
;; a code block
(defstruct code-block-tok lang body)
(defstruct bullet-tok body)

(defun safe-read-char (stream)
  (read-char stream nil :eof))

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

(defun string-postfixesp (larger smaller)
  "Determine whether the assumed smaller string postfixes the larger."
  (let ((sm-len (length smaller))
        (lg-len (length larger)))
    (and
     (>= lg-len sm-len)
     (string= smaller larger :start2 (- lg-len sm-len) :end2 lg-len))))

(defun without-postfix (larger smaller)
  "Produce the larger string without its assumed postfix, smaller"
  (subseq larger 0 (- (length larger) (length smaller))))

(defun take-until (stream end-on)
  "Take until we get a specific char or string"
  (if (stringp end-on)
      (take-until-string stream end-on)
      (take-until-char stream end-on)))

(defun take-until-string (stream end-on)
  "take from a stream until a particular character is received"
  (let ((chars (make-adjustable-string "")))
    (loop for next-char = (safe-read-char stream)
          until (or (eq next-char :eof)
                    (string-postfixesp chars end-on))
          do (push-char chars next-char))
    (without-postfix chars end-on)))

(defun take-until-char (stream end-on)
  "take from a stream until a particular character is received"
  (let ((chars (make-adjustable-string "")))
    (loop for next-char = (safe-read-char stream)
          until (or (eq next-char end-on)
                    (eq next-char :eof)
                    (and (stringp end-on)
                         (string-postfixesp chars end-on)))
          do (push-char chars next-char))
    chars))

(defun tokenize (stream)
  "parse a line from a stream, assuming we're at the start of a line, then continue"
  (let ((char (safe-read-char stream)))
    (if (eq char :eof)
        ()
        (cons
         (case char
           ((#\newline) :ignore)
           ((#\#) (tokenize-macro-line-or-comment stream))
           ((#\*) (tokenize-heading stream))
           ((#\-) (tokenize-bullet stream))
           (otherwise (tokenize-text char stream)))
         (tokenize stream)))))

(defun tokenize-bullet (stream)
  "Make a bullet token."
  (make-bullet-tok
   :body (text-tok-body (tokenize-text nil stream))))

(defun tokenize-macro-line-or-comment (stream)
  "Tokenize a macro line or comment"
  (let ((cmd (take-until stream #\space)))
    (string-case
        (cmd)
      ("+TITLE:" (parse-title stream))
      ("+BEGIN_SRC" (parse-code-block stream))
      (t "Not sure what this macro is"))))

(defun parse-title (stream)
  "Parse a document title from a stream."
  (let ((title-text (take-until stream #\newline)))
    (make-title-tok :title title-text)))


(defun parse-code-block (stream)
  "Parse a code block from a stream."
  (let ((lang (take-until stream #\newline))
        (body (take-until stream "#+END_SRC")))
    (make-code-block-tok
      :lang lang
      :body body)))


(defun tokenize-text (last-char stream)
  "Tokenize a text node until EOL. Append the char if it exists."
  (let ((body (take-until stream #\newline)))
    (make-text-tok :body (if last-char (concatenate 'string (list last-char) body) body))))


(defun tokenize-heading (stream)
  "tokenize an Org-mode heading"
  (let ((header-rank (length (take-until stream #\SPACE))) ; the rank of the current header
        (title (take-until stream #\newline))) ; the title of the current header
    (make-header-tok
     :rank header-rank
     :title title)))


;; the input test file:
;; (defconstant +test-path+ "./README.org")

;; the expected file format:
;; (defconstant +expected-readme-out+
;;   (make-file
;;    :title "jake.chvatal.com"
;;    :metadata (make-hash-table)
;;    :body (list
;;           (list
;;            'newline
;;            "This is the index of my personal website found "
;;            (make-link :text "here" :url "https://jake.isnt.online")
;;            "."
;;            'newline
;;            "100% score on the "
;;            (make-link :text "Lighthouse audit" :url "https://foo.software/lighthouse")
;;            "."
;;            'newline)

;;           (make-heading
;;            :title "Goals"
;;            :body (list
;;                   (make-bullet :text "Personal landing page with links")
;;                   (make-bullet :text "No external resources loaded")
;;                   (make-bullet :text "SEO Optimized")
;;                   (make-bullet :text "Ten packets (to load instantly)")
;;                   'newline))

;;           (make-heading
;;            :title "Running"
;;            :body (list
;;                   "To render the website:"
;;                   (make-code :lang "sh" :body "lein run > index.html")
;;                   'newline
;;                   "The website's written in Clojure because it provides, IMO, the most ergonomic HTML DSL available, `hiccup`. Though other languages provide similar facilities, Clojure provides the nicest syntax without incurring the up-front development costs associated with strong types or legacy work associated with other lisps."
;;                   )))))
