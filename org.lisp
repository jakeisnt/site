(load "~/quicklisp/setup.lisp")

(defpackage parse-org
  (:use :cl :trivia))

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
(defstruct heading title body)
;; (heading-p (make-header :title "asdf" :body "asdf"))

;; a link with text and a url
(defstruct link text url)

;; parse an org-mode file to an internal, struct-based representation
(defun parse (fname)
  (with-open-file (stream fname :direction :input :if-does-not-exist nil)
    (if stream (parse-stream stream))))

;; parse a stream, assuming we're at the start of a line
(defun parse-line (stream)
  (let ((char (read-char stream)))
    (case char
      ((#\#) (parse-macro-line-or-comment stream))
      ((#\*) (parse-heading stream))
      ((#\-) (parse-heading stream))
      (otherwise (parse-text stream)))))

;; interpret a bullet point (a heading)
(defun parse-heading (stream)
  (read-char stream)
  (let ((bullet-text (take char until eol)))
    (make-bullet :text bullet-text)))

;; take from the stream until a particular character is received
(defun take-until (stream end-on)
  (let ((num-chars-taken 0))
    (loop for next-char = (read-char stream nil :eof)
          until (eq next-char end-on)
          do (setf num-chars-taken (1+ num-chars-taken)))
    num-chars-taken))

(defun parse-heading (stream)
  ;; take characters until we hit a newline; this becomes our title
  (let ((header-rank (length (take-until stream #\SPACE))) ;; number
        (title (take-until stream #\n))                    ;; string
        (next-header-result (parse-line stream))) ;; (list of rest of file)


    ;; take from the next header
    (if (<= header-rank (header-rank current-header))
        (list
         current-header

         (make-header
          :rank header-rank
          :title title
          :body ...)) ;; append to current header




        )

    ;; problem: what happens if the next line is a heading and has a lower level than us?
    ;; we don't want to make that a child of this!
    ;; solution: parse lines until we get a heading-p back with a lower priority than us.
    ;; this tells us when we append rather than nesting into the body of this header.
    ;; we then return the whole list of things here.

    ;; real solution: we pass a continuation that they invoke;
    ;; we want to give them the opportunity to take two paths:
    ;; - adding to this header (so we need the header 'rank'
    ;; - escaping this header to the main list

    ;; - with mutation, we just pass the header and let them either setf the list to append to it or return,
    ;; - and we take the return value
    ;; - without mutation, we
    ))



;; the input test file:
(defconstant +test-path+ "./README.org")

;; the expected file format:
(defconstant +expected-readme-out+
  (make-file
   :title "jake.chvatal.com"
   :metadata (make-hash-table)
   :body (list
          (list
           'newline
           "This is the index of my personal website found "
           (make-link :text "here" :url "https://jake.isnt.online")
           "."
           'newline
           "100% score on the "
           (make-link :text "Lighthouse audit" :url "https://foo.software/lighthouse")
           "."
           'newline)

          (make-heading
           :title "Goals"
           :body (list
                  (make-bullet :text "Personal landing page with links")
                  (make-bullet :text "No external resources loaded")
                  (make-bullet :text "SEO Optimized")
                  (make-bullet :text "Ten packets (to load instantly)")
                  'newline))

          (make-heading
           :title "Running"
           :body (list
                  "To render the website:"
                  (make-code :lang "sh" :body "lein run > index.html")
                  'newline
                  "The website's written in Clojure because it provides, IMO, the most ergonomic HTML DSL available, `hiccup`. Though other languages provide similar facilities, Clojure provides the nicest syntax without incurring the up-front development costs associated with strong types or legacy work associated with other lisps."
                  )))))
