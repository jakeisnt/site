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

;; current problem:
;; -> we hit a char, and stop on it
;; -> then we don't skip that char when we continue to parse



(defun take-until (stream end-on)
  "Take until we get a specific char or string;
if we find it, return ('found str);
if we don't, return ('missing str)
"
  (if (stringp end-on)
      (take-until-string stream end-on)
      (take-until-char stream end-on)))



;; note: this doesnt always work cos always len of the first;
;; should be the length of whatever we end on
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
    (string-case
        (cmd)
      ("+TITLE:" (parse-title stream))
      ("+BEGIN_SRC" (parse-code-block stream t))
      ("+begin_src" (parse-code-block stream nil))
      (t "Not sure what this macro is"))))

(defun parse-link (stream)
  "Parse a link with a possible title and mandatory URL"
  (let* ((link-text (take-until stream "]]"))
         (body (cl-ppcre::split "\\]\\[" link-text)))
    (if (eq (length body) 2)
        (defs::make-link :title (cadr body) :url (car body))
        (defs::make-link :url (car body)))))


;; take-until can terminate on eof before we find that last character
;; we should explicitly be informed if this happens;
;; currently, it'll chunk everything after some starting character until eol
;; into the constructor of one of these things,
;; rather than giving up and returning text.
;; we should see if we actually found the closing char,
;; and just return a string if we didn't
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

(defun make-naive-link (txt)
  ;; http was cut so we manually add it backlol
  (defs::make-link :url (concatenate 'string "http" txt)))


;; 'chars' are the chars to be cut from the end of our recognizing buffer,
;; so they're the chars we match on.
;; the rest of them will be pulled off later
;; there is definitely a way to handle this in a more elegant, generic way2
(defmacro tapp2 (parser chars)
  `(let ()
     (setq res (cons (,parser stream)
                     ;; NOTE: this only works because we watn everyting to have the same len,
                     ;; and without postfix uses len to do this
                     (cons (util::without-postfix buffer ,chars) res)))
     (setq buffer (make-adjustable-string ""))))

;; semantics of these inlines:
;; if we hit a space or newline or eof before we find closing,
;; we give up on the form and just return the text.

(defun parse-naive-link (stream)
  "Parse a link with a possible title and mandatory URL"
  (let* ((link-text (take-until-or stream #\space #\newline)))
    (make-naive-link link-text)))

;; stream and buffer are assumed to be in scope here
(defmacro tapp (make chars)
  `(let ((parsed (parse-char stream ,make ,chars))
         (sin-postfix (if (util::string-postfixesp buffer ,chars)
                          (util::without-postfix buffer ,chars)
                          buffer)))
     (setq res (cons parsed (cons sin-postfix res)))
     (setq buffer (make-adjustable-string ""))))

(defun make-bold (txt)
  (defs::make-bold :text txt))

(defun make-ital (txt)
  (defs::make-ital :text txt))

(defun make-verb (txt)
  (defs::make-verb :text txt))

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
