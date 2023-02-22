(load "~/quicklisp/setup.lisp")

(load "~/site/src/util.lisp")
(load "~/site/src/org/ast.lisp")

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
;; a bullet point
(defstruct bullet-tok body)

;; an ID with a string
(defstruct id-tok id)

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

(defun in (elem lst)
  "is the element in the list?"
  (member elem lst :test #'equal))

(defun take-until-chars (stream could-end-on)
  "take from a stream until any of the characters are received"
  (let ((chars (make-adjustable-string "")))
    (loop for next-char = (safe-read-char stream)
          until (or (in next-char could-end-on)
                    (eq next-char :eof))
          do (push-char chars next-char))
    chars))

(defun remove-spaces (str)
   (do* ((stringstream (make-string-input-stream str))
         (result nil (cons next result))
         (next (read stringstream nil 'eos)
               (read stringstream nil 'eos)))
        ((equal next 'eos) (reverse result))))

(remove-spaces "as df")

(defun tokenize-properties (stream)
  "Tokenize a property drawer."

  (let ((pre (take-until stream "ID:"))
        (id (remove-spaces (take-until-char stream #\newline)))
        (post (take-until stream ":END:")))

    (print pre)
    (print id)
    (print post)
    (make-id-tok :id id)))

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
    (ast::make-code-block :lang lang :body body)))

;; NOTE: this is very broken and makes some bad assumptions about org files.
(defun parse-quote-block (stream cap)
  "Parse a quote block from a stream."
  (let* ((body (take-until stream #\—)) ; this is bad. can't use em dash in quotes.
         (author (take-until stream #\newline)) ; this can be a link or a quote?
         (rst (take-until stream (if cap "#+END_QUOTE" "#+end_quote"))))
    (ast::make-quote-block :body body :author (tokenize-line author '()))))

(defun split-first (line look-for)
  "Split the line on the first occurence of the character."
  (split look-for line :limit 2))

(defun tokenize-macro-line-or-comment (stream)
  "Tokenize a macro line or comment. We've already seen a '#' at the start of the line."
  (let ((cmd (take-until-chars stream (list #\space #\newline))))
    (string-case (cmd)
      ("+TITLE:"    (parse-title stream))
      ("+title:"    (parse-title stream))
      ("+BEGIN_SRC" (parse-code-block stream t))
      ("+begin_src" (parse-code-block stream nil))
      ("+BEGIN_QUOTE" (parse-quote-block stream t))
      ("+begin_quote" (parse-quote-block stream nil))
      (t "Not sure what this macro is"))))

;; Assuming we've found matching parens,
;; give their contents to these functions to make the respective construct.
;; TODO: support in-file links and links to other local files
(defun make-link (link-text)
  "Parse a link with a possible title and mandatory URL"
  (let* ((body (cl-ppcre::split "\\]\\[" link-text)))
    (if (eq (length body) 2)
        (ast::make-link :title (cadr body) :url (car body))
        (ast::make-link :url (car body)))))

(defun make-naive-link (txt)
  ;; http was cut so we manually add it backlol
  (ast::make-link :url (concatenate 'string "http" txt)))

(defun make-bold (txt)
  (ast::make-bold :text txt))

(defun make-ital (txt)
  (ast::make-ital :text txt))

(defun make-verb (txt)
  (ast::make-verb :text txt))

(defun make-latex (txt)
  txt)

(defun apply-first (pair fn)
  (cons (funcall fn (car pair)) (cdr pair)))


(defun try-find (line open-str close-str make-obj)
  "Try to find a matching pair on the line,
   converting the found split into an object if we find it"

  ;; somewhere here, we are returning a cons, and in other cases we return a list
  (if (util::string-prefixesp line open-str)
      (let* ((without-prefix (util::without-prefix line open-str))
             (without-prefix-len (length without-prefix)))
        (if close-str
            (let ((maybe-split (split-first without-prefix close-str)))
              ;; if we successfully split on the char,
              (if (eq 2 (length maybe-split))
                  ;; apply the constructor to the first arg!
                  (apply-first maybe-split make-obj)
                  ;; otherwise,
                  (let ((leftover-string (car maybe-split)))
                    ;; if we found our thing at the last pos on the line,
                    (if (not (eq without-prefix-len (length leftover-string)))
                        ;; then we still make the object.
                        (list (funcall make-obj leftover-string) "")
                        ;; otherwise, we found nothing.
                        (list nil leftover-string)))))
            ;; if we don't have a postfix to look for, we take the rest of the line naively
            (list (funcall make-obj without-prefix) "")))
      (list nil line)))

(defmacro fd (open-str close-str make-obj else)
  `(let ((maybe-found (try-find text-line ,open-str ,close-str ,make-obj)))
     (if (car maybe-found)
         (let ((remaining-string (car (cdr maybe-found)))
               (new-acc (cons (car maybe-found) acc)))
           (tokenize-line remaining-string new-acc))
         ,else)))

(defun fuse-subseq (acc cur)
  (if (and (car acc) (stringp (car acc)))
      (cons
       (concatenate 'string (car acc) cur)
       (cdr acc))
      (cons cur acc)))

;; Start with the current string line.
;; Check the first characters, starting at [0],
;;   for a match with the top pattern.
;; If a match is found, look for the closing pattern.
;; If we find the closing pattern,
;;   produce the struct created with this pattern
;;   and the rest of the string.
;; If we do not find the closing pattern,
;;   produce nil and the current string. Then try the next pattern.

;; If we fail to match any of these patterns,
;;   pop the current character off of the text line.
(defun tokenize-line (text-line acc)
  (if (or (not text-line) (eq 0 (length text-line)))
      (reverse acc)
      (fd "http" " " #'make-naive-link
          (fd "http" (string #\newline) #'make-naive-link
              ;; TODO: determining when to stop parsing a naive link is really hard.
              ;; i should look at the org mode source code for this.
              ;; it might be best just to parse these as plain text too.
              (fd "http" nil #'make-naive-link
                  (fd "[[" "]]" #'make-link
                      (fd "\\*" "\\*" #'make-bold
                          (fd "/" "/" #'make-ital
                              (fd "`" "`" #'make-verb
                                  ;; NOTE: This is broken and not capturing my latex at all.
                                  (fd "$" "$" #'make-latex
                                      (tokenize-line
                                       (subseq text-line 1)
                                       (fuse-subseq acc (subseq text-line 0 1)))))))))))))


(defun tokenize-text (last-char stream)
  "Tokenize a text node until EOL. Append the char if it exists."
  (let*
      ((body (take-until stream #\newline))
       (fixed-body (if last-char (concatenate 'string (list last-char) body) body))
       (tokenized-body (tokenize-line fixed-body '())))
    (make-text-tok :body tokenized-body)))


(defun tokenize-heading (stream)
  "tokenize an Org-mode heading"
  (let ((header-rank (length (take-until stream #\SPACE))) ; the rank of the current header
        (title (take-until stream #\newline))) ; the title of the current header
    (make-header-tok
     :rank header-rank
     :title title)))
