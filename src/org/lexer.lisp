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
  "Take until we get a specific char or string"
  (if (stringp end-on)
      (take-until-string stream end-on)
      (take-until-char stream end-on)))

(defun take-until-string (stream end-on)
  "take from a stream until a particular character is received"
  (let ((chars (make-adjustable-string "")))
    (loop for next-char = (safe-read-char stream)
          do (push-char chars next-char)
          until (or (eq next-char :eof)
                    (util::string-postfixesp chars end-on)))
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


  (defun tokenize-text-until (text-line)
    "Tokenize text and find special cool things in it"
    (with-open-stream (stream (make-string-input-stream text-line))
      (let ((res ())
            (buffer (make-adjustable-string "")))
        (loop for next-char = (safe-read-char stream)
              until (eq next-char :eof)
              do (let ()
                   (push-char buffer next-char)
                   ;;  if we find a link,parse the rest of the string from it and reset the buffer
                   (if (util::string-postfixesp buffer "[[")
                       (let ()
                        (setq res (cons (parse-link stream)
                                        (cons
                                         (util::without-postfix buffer "[[")
                                         res)))
                        (setq buffer (make-adjustable-string ""))))))
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
