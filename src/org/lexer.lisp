(load "~/quicklisp/setup.lisp")

(load "./src/util.lisp")
(load "./src/org/defs.lisp")

(ql:quickload :string-case)
(ql:quickload :cl-ppcre)

(defpackage lexer
  (:use :cl :string-case :cl-ppcre))

(in-package lexer)

;; A header token with a rank and title
(defstruct header-tok rank title)
;; A text token with a body
(defstruct text-tok body)
;; a document title
(defstruct title-tok title)
;; a code block
(defstruct code-block-tok lang body)
;; a bullet point
(defstruct bullet-tok body)

(defun safe-read-char (stream)
  "Safely read a character from a stream"
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

(defun take-until (stream end-on)
  "Take until we get a specific char or string;
   if we find it, return ('found str);
   if we don't, return ('missing str)
  "
  (if (stringp end-on)
      (take-until-string stream end-on)
      (take-until-char stream end-on)))

;; note: this doesn't always work cos always len of the first;
;; should be the length of whatever we end on lol...
;; also this should be extended an arbitrary number of `end-on`s,
;; either characters or strings,
;; which would let us fuse all of these `take-until`s lol. with a macro!
(defun take-until-or (stream end-on end-on2)
  "take from a stream until a particular character is received"
  (let ((chars (make-adjustable-string "")))
    (loop for next-char = (safe-read-char stream)
          until (or (eq next-char end-on)
                    (eq next-char end-on2)
                    (eq next-char :eof)
                    (and (stringp end-on)
                         (util::string-postfixesp chars end-on)
                         (util::string-postfixesp chars end-on)))
          do (push-char chars next-char))
    chars))

(defun take-until-string (stream end-on)
  "take from a stream until a particular character is received
   omits the string we terminate on"
  (let ((chars (make-adjustable-string "")))
    (loop for next-char = (safe-read-char stream)
          until (or (eq next-char :eof)
                    (util::string-postfixesp chars end-on))
          do (when (not (eq next-char :eof))
               (push-char chars next-char)))
    (util::without-postfix chars end-on)))

(defun take-until-char (stream end-on)
  "take from a stream until a particular character is received"
  (let ((chars (make-adjustable-string "")))
    (loop for next-char = (safe-read-char stream)
          until (or (eq next-char end-on)
                    (eq next-char :eof)
                    (and (stringp end-on)
                         (util::string-postfixesp chars end-on)))
          do (push-char chars next-char))
    chars))

(defun tokenize-properties (stream)
  "Tokenize a property drawer."
  (take-until stream ":END:")
  :ignore)

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
           ((#\:) (tokenize-properties stream)) ; TODO: not guaranteed here
           (otherwise (tokenize-text char stream)))
         (tokenize stream)))))

(defun tokenize-bullet (stream)
  "Make a bullet token."
  (make-bullet-tok
   :body (text-tok-body (tokenize-text nil stream))))

(defun parse-title (stream)
  "Parse a document title from a stream."
  (let ((title-text (take-until stream #\newline)))
    (make-title-tok :title title-text)))


(defun parse-code-block (stream cap)
  "Parse a code block from a stream."
  (let ((lang (take-until stream #\newline))
        (body (take-until stream (if cap  "#+END_SRC" "#+end_src"))))
    (make-code-block-tok
     :lang lang
     :body body)))

(defun tokenize-macro-line-or-comment (stream)
  "Tokenize a macro line or comment"
  (let ((cmd (take-until stream #\space)))
    (string-case (cmd)
      ("+TITLE:"    (parse-title stream))
      ("+BEGIN_SRC" (parse-code-block stream t))
      ("+begin_src" (parse-code-block stream nil))
      (t "Not sure what this macro is"))))

(defun parse-char (stream make chars)
  (let ((bold-cont (take-until stream chars)))
    (funcall make bold-cont)))

;; buffer assumed to be in scope here
(defmacro tfit (chars)
  `(util::string-postfixesp buffer ,chars))


;; ideal ux:
;; (scan-match
;;  (("[[" mb-link "]]") parse-link)
;;  (("*" bold "*") make-bold)
;;  (("/" ital "/") make-ital)
;;  (("`" verb "`") make-verb))
;; semantics: prefer first occuring match, prefer longer match
;; i wonder if we can sense for conflicts like parser generators in this grammar!
;; that would be very cool;.


;; 'chars' are the chars to be cut from the end of our recognizing buffer,
;; so they're the chars we match on.
;; the rest of them will be pulled off later
;; there is definitely a way to handle this in a more elegant, generic way
;; (see spec above)
;; but i'm not entirely sure offhand how to encode it
(defmacro tapp2 (parser chars)
  `(let ()
     (setq res (cons (,parser stream)
                     ;; NOTE: this only works because we watn everyting to have the same len,
                     ;; and without postfix uses len to do this
                     (cons (util::without-postfix buffer ,chars) res)))
     (setq buffer (make-adjustable-string ""))))

;; stream and buffer are assumed to be in scope here
(defmacro tapp (make chars)
  `(let ((parsed (parse-char stream ,make ,chars))
         (sin-postfix (if (util::string-postfixesp buffer ,chars)
                          (util::without-postfix buffer ,chars)
                          buffer)))
     (setq res (cons parsed (cons sin-postfix res)))
     (setq buffer (make-adjustable-string ""))))


;; --- Makers ---
;; Assuming we've found matching parens,
;; give their contents to these functions to make the respective construct.
(defun make-link (link-text)
  "Parse a link with a possible title and mandatory URL"
  (let* ((body (cl-ppcre::split "\\]\\[" link-text)))
    (if (eq (length body) 2)
        (defs::make-link :title (cadr body) :url (car body))
        (defs::make-link :url (car body)))))

(defun make-naive-link (txt)
  ;; http was cut so we manually add it backlol
  (defs::make-link :url (concatenate 'string "http" txt)))

(defun make-bold (txt)
  (defs::make-bold :text txt))

(defun make-ital (txt)
  (defs::make-ital :text txt))

(defun make-verb (txt)
  (defs::make-verb :text txt))

;; (defun split-on-first (line)
;;   "Split a line on the first occurence of a substring supposedly in that line"
;;   (loop named find-substr
;;         for idex from 0 to (- no-prefix-len 1) ; we might be over indexing
;;         with str-end-idx = (min no-prefix-len (+ idex close-len))
;;         do (let ((cur-substr (subseq line-no-prefix idex str-end-idx)))
;;              (when (eq cur-substr close-str)
;;                (return-from
;;                 find-substr
;;                  (cons
;;                   (subseq line-no-prefix 0 idex)
;;                   (subseq line-no-prefix idx)))))))

(defun split-first (line look-for)
  (split look-for line :limit 2))

(defun apply-first (pair fn)
  (cons (funcall fn (car pair)) (cdr pair)))


(defun try-find (line open-str close-str make-obj)
  (if (util::string-prefixesp line open-ls)
      ;; look for closing chars
      ;; do the thing
      (let* ((line-no-prefix (util::without-prefix line open-ls))
             (no-prefix-len (length line-no-prefix))
             (close-len (length close-str))
             (maybe-substr (split-on-closing )))

        (if (consp maybe-substr) (apply-first maybe-substr make-obj) (cons nil line)))
      (cons nil line)))

(defun tokenize-line (text-line)
  "http" (or #\newline #\space)
  "[[" "]]"
  "*" "*"
  "/" "/"
  "`" "`"
  "$" "$"
  )

;; if we see the opening char:
;; find the closing char
;; if we find the closing char,
;; split on it and return the split list;
;; otherwise, return nil and the original

;; 1. Start with the current string line.
;; 2. Check the first characters, starting at [0],
;;    for a match with the top pattern.
;; 3. If a match is found, look for the closing pattern.
;;    If we find the closing pattern,
;;      produce the struct created with this pattern
;;      and the rest of the string.
;;    If we do not find the closing pattern,
;;      produce nil and the current string. Then try the next pattern.

;; If we fail to match any of these patterns,
;;   pop the current character off of the text line.


(defun tokenize-text-until (text-line)
  "Tokenize text and find special cool things in it"
  (with-open-stream (stream (make-string-input-stream text-line))
    (let ((res ())
          (buffer (make-adjustable-string "")))
      (loop for next-char = (safe-read-char stream)
            until (eq next-char :eof)
            do (let ()
                 (push-char buffer next-char)
                 (cond
                   ((tfit "http") (tapp2 parse-naive-link "http")) ;we need an 'or space newline' here
                   ((tfit "[[") (tapp2 parse-link "[["))
                   ((tfit "*")  (tapp #'make-bold "*"))
                   ((tfit "/")  (tapp #'make-ital "/"))
                   ((tfit "`")  (tapp #'make-verb "`"))
                   ((tfit "$")  (tapp #'make-verb "$")))))
      (reverse (cons buffer res)))))


(defun tokenize-text (last-char stream)
  "Tokenize a text node until EOL. Append the char if it exists."
  (let*
    ((body (take-until stream #\newline))
     (fixed-body (if last-char (concatenate 'string (list last-char) body) body))
     (tokenized-body (tokenize-text-until fixed-body)))
    (make-text-tok :body tokenized-body)))

(defun tokenize-heading (stream)
  "tokenize an Org-mode heading"
  (let ((header-rank (length (take-until stream #\SPACE))) ; the rank of the current header
        (title (take-until stream #\newline))) ; the title of the current header
    (make-header-tok
     :rank header-rank
     :title title)))
