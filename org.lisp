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

(defun parse (fname)
  "parse an org-mode file to an internal, struct-based representation"
  (with-open-file (stream fname :direction :input :if-does-not-exist nil)
    (if stream (parse-stream stream) 'no-stream)))

(defun parse-line (stream cur-rank)
  "parse a line from a stream, assuming we're at the start of a line, then continue"
  (let ((char (read-char stream)))
    (case char
      ((#\#) (parse-macro-line-or-comment stream cur-rank))
      ((#\*) (parse-heading stream cur-rank))
      ((#\-) (parse-bullet stream))
      (otherwise (parse-text stream)))))


;; how do we approach this in a single pass?
;; we need to give the child some way to add itself to me and produce me,
;;  but also need some way to make a children of this

;; this is my monad
;; it accumulates the entire result in a closure
;; we close over the current header at every time? is that correct?


;; we have the linear scan to tokenize, but how do we build it back up in a tail recursive fashion?
;; what if we go backwards?

;; i.e.:
;; we see text, we add text to list and continue
;; we see a heading, we add text to body of heading and continue with heading as the list
;; we see a smaller heading, we
;; yes! this works. let's split tokenization and parsing


(lambda (header new-elem)
  (cond
    ((header-p new-elem)
     (if (< (:rank header) (:rank new-elem))
         )
     ))

  (if (< (:rank header)))
  (make-header
   :rank  header-rank
   :title title
   :body  (append body (list new-elem))))

;; this has to both produce a valid header at every step and add the given argument to the end of the header


(defun parse-bullet (stream cur-rank)
  "parse a bullet, assuming we've just seen a bullet point"
  (read-char stream)     ; skip the first char; it's blank according to the spec
  (let ((bullet-text (take-until #\n))) ; take characters until we hit a newline
    (make-bullet :text bullet-text)))   ; make the bullet with those characters!

(defun take-until (stream end-on)
  "take from a stream until a particular character is received"
  (let ((num-chars-taken 0))
    (loop for next-char = (read-char stream nil :eof)
          until (eq next-char end-on)
          do (setf num-chars-taken (1+ num-chars-taken)))
    num-chars-taken))

(defun parse-heading (stream cur-rank)
   "Parse an Org-mode heading"
  (let ((header-rank (length (take-until stream #\SPACE))) ; the rank of the current header
        (title (take-until stream #\n)) ; the title of the current header
        (next-header-result (parse-line stream))) ; the rest of the file, parsed

    ;; take from the next header
    (if (<= header-rank (header-rank current-header))
        (cons
         (make-header
          :rank header-rank
          :title title
          :body ...)
         next-header-result))))



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
