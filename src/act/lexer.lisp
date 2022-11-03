;; This file is a compiler for an acting script
;; text-based format (found at the specified location
;; in the wiki below, with the hardcoded rendering logic)
;; to a view of the script as a fake text conversation
;; as an independent HTML page.


;; An AST that represents conversations.
(defpackage act-ast
  (:use :cl))

(in-package act-ast)

;; A single message from a person
(defstruct message author text)

;; Some context for the scene.
;; Concretely: (Context.)
;; This can also be inline
(defstruct context text)

;; An italicised section: /ital/.
(defstruct ital text)

;; An unstructured body text portion.
(defstruct body text)

;; A name alias in the body. TODO. Some cool ideas here.
(defstruct alias shorthand name)

;; an entire script
(defstruct script characters body)


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

(defun parse-message-body-help (body-stream currently-ital)
  "
   Parse the body of a message, looking for character emphasis.
   Assumes it finds.
  "
  (let* ((red-line (parse-help::take-until-char body-stream #\/))
         (is-eof (equal (car red-line) :eof))
         (next-node (cdr red-line)))
    (cons (if currently-ital
              (act-ast::make-ital :text next-node)
              (act-ast::make-body :text next-node))
          (if is-eof
              nil
              (parse-message-body-help body-stream (not currently-ital))))))

(defun parse-message-body (body-text)
  "Parse the message body."
  (let* ((body-stream (make-string-input-stream body-text))
         (res (parse-message-body-help body-stream nil)))
    res))

(defun parse-message (line)
  "Parse a message as a line."
  (let ((line-parts (split ": " line :limit 2)))
    (act-ast::make-message
     :author (car line-parts)
     :text (parse-message-body (cadr line-parts)))))

(defun parse-alias (line)
  "Parse a message as a line."
  (let ((line-parts (split "=" line :limit 2)))
    (act-ast::make-alias
     :shorthand (car line-parts)
     :name (cadr line-parts))))

(defmacro match-line (recur stream cnd)
  "Abstract out the practice of matching on a line and looking for something."
  `(let ((line (parse-help::safe-read-line stream)))
     (if (eq :eof line)
         nil
         (let ((readline-res ,cnd))
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
     ((parse-help::can-split-on #\= line) (parse-alias line))
     ((parse-help::can-split-on #\= line) :ignore) ;; (parse-alias stream)
     (t :ignore))))

(defun extract-list-elements (ls pred)
  "Remove elements from the list that satisfy a predicate."
  (let ((characters ())
        (rest-of-script ()))
    (loop for elem in (reverse ls)
          do (if (funcall pred elem)
                 (setq characters (cons elem characters))
                 (setq rest-of-script (cons elem rest-of-script))))
    (values characters rest-of-script)))

(defun extract-characters (script-list)
  "Split a list on the characters of the script."
  (extract-list-elements script-list #'act-ast::alias-p))

(defun parse-script (stream)
  (multiple-value-bind
        (characters rest-of-script) (extract-characters (parse stream))
    (act-ast::make-script
     :characters characters
     :body rest-of-script)))

;; HTML serializer for the 'act' language

(load "~/site/src/html.lisp")
(load "~/site/src/util.lisp")

(ql:quickload :spinneret)

(defpackage act-html
  (:use :cl))

(in-package act-html)

(ql:quickload :cl-ppcre)

(defun render-message-text (txt)
  (loop for item in txt
        collect
        (spinneret::with-html
          (cond
            ((act-ast::ital-p item) (:i (act-ast::ital-text item)))
            ((act-ast::body-p item) (:span (act-ast::body-text item)))))))

(defun first-character-alias (script)
  (act-ast::alias-shorthand (car (act-ast::script-characters script))))

(defun render-node (node script)
  (spinneret::with-html
    (cond
      ((act-ast::message-p node)
       (:blockquote
        :class (concatenate
                'string
                "message "
                (if (equal (act-ast::message-author node) (first-character-alias script)) "a" "b"))
        (:p :class "message-text" (render-message-text (act-ast::message-text node)))))
      ((act-ast::context-p node)
       (:p
        :class "context"
        (act-ast::context-text node)))
      (t nil))))

(defun conversation-page (script)
  (htmx::body
   "Conversation"
   nil
   (:article
    :class "conversation"
    (loop for node in (act-ast::script-body script)
          collect (render-node node script)))
   nil))

(defun render-script (path)
  (with-open-file (stream path :direction :input)
    (util::write-file
     "/home/jake/site/docs/fool-for-love.html"
     (conversation-page (act-parser::parse-script stream)))))



;; (act-html::render-script "/home/jake/wiki/etc/fool-for-love.act")
