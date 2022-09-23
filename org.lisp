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

(defstruct file title metadata body)
(defstruct heading title body)
(heading-p (make-header :title "asdf" :body "asdf"))

(defstruct link text url)

;; parse an org-mode file
(defun parse (fname)
  (with-open-file (stream fname :direction :input :if-does-not-exist nil)
    (if stream (parse-stream stream))))

;; parse a stream assuming we're at the start of a line
(defun parse-line (stream)
  (let ((char (read-char stream)))
    (case char
      ((#\#) (parse-macro-line-or-comment stream))
      ((#\*) (parse-heading stream))
      ((#\-) (parse-bullet stream))
      (otherwise (parse-text stream)))))

(defun parse-bullet (stream)
  (read-char stream)
  (let ((bullet-text (take char until eol)))
    (make-bullet :text bullet-text)))

(defun parse-heading (stream)
  ;; toss the first char


  ;; take characters until we hit a newline
  ;; this becomes our title
  (let ((header-rank (+ 1 (take stars until space))) ;; number
        (title (take char until newline))            ;; string
        (next-header-result (parse-line stream)))    ;; (list of rest of file)


    ;; this is where we provide the 'exit' continuation, right?
    ;; if the user
    (if (<= header-rank (rank current-header))
        (list
         current-header

         (make-header :title title :body ...))
        (list
         ;; append to current header


         ))

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
