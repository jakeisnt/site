(load "~/quicklisp/setup.lisp")

(defpackage parse-org
  (:use :cl))

(in-package parse-org)


;; what does our file have?
;; file: title, metadata, body
;; body: listof (or heading text code)
;; heading: title, body
;; text: listof 
;; code: language, contents

;; The contents of an org-mode file
;; (defstruct file title metadata body)

;; A heading with its corresponding body
;; (defstruct heading title body)
;; (heading-p (make-header :title "asdf" :body "asdf"))

;; a link with text and a url
;; (defstruct link text url)

(defun parse (fname)
  "parse an org-mode file to an internal, struct-based representation"
  (with-open-file (stream fname :direction :input :if-does-not-exist nil)
    (if stream (tokenize stream) 'no-stream)))

;;  --- Tokenize ---

;; A header token with a rank and title
(defstruct header-tok rank title)
;; A text token with a body
(defstruct text-tok body)

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
  "take from a stream until a particular character is received"
  (let ((chars (make-adjustable-string "")))
    (loop for next-char = (read-char stream nil :eof)
          until (or (eq next-char end-on) (eq next-char :eof))
          do (push-char chars next-char))
    chars))

(defun tokenize (stream)
  "parse a line from a stream, assuming we're at the start of a line, then continue"
  (print "parsing line")
  (let ((char (read-char stream)))
    (case char
      ;; ((#\#) (tokenize-macro-line-or-comment stream))
      ((#\*) (tokenize-heading stream))
      ;; ((#\-) (tokenize-bullet stream))
      (otherwise (tokenize-text stream)))))

(defun tokenize-text (stream)
  "Tokenize a text node until EOL"
  (let ((body (take-until stream #\n)))
    (cons
     (make-text-tok :body body)
     (tokenize stream))))

;; this has to both produce a valid header at every step and add the given argument to the end of the header
;; (defun tokenize-bullet (stream)
;;   "tokenize a bullet, assuming we've just seen a bullet point"
;;   (read-char stream)     ; skip the first char; it's blank according to the spec
;;   (let ((bullet-text (take-until #\n))) ; take characters until we hit a newline
;;     (make-bullet-tok :text bullet-text)))   ; make the bullet with those characters!
(defun tokenize-heading (stream)
  "tokenize an Org-mode heading"
  (let ((header-rank (length (take-until stream #\SPACE))) ; the rank of the current header
        (title (take-until stream #\n))) ; the title of the current header
    (cons
     (make-header-tok
      :rank header-rank
      :title title)
     (tokenize stream))))



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
