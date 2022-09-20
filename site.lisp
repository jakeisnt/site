(load "~/quicklisp/setup.lisp")

(ql:quickload :flute)
(ql:quickload :flute-test)

(defpackage flute-user
  (:use :cl :flute))

;; https://gist.github.com/privet-kitty/e5abd3f09485c5828af81ad02e8c2baf
;; worth looking into;;


;; TODO: make a macro to reduce noise here
;; (defmacro defvars)
(defmacro defv (name val)
  `(defvar ,(read-from-string (format nil "~{~a~}" name)) ,val))

(defv name "jake")

(defvar *name* "Jake Chvatal")
(defvar *email* "jake+site@isnt.online")
(defvar *twitter* "@jakeissnt")
(defvar *instagram* "jakeisnt")
(defvar *mastodon* "jakeisnt")
(defvar *github* "jakeisnt")
(defvar *reddit* "jakeisnt")
(defvar *arena* "jake-isnt")
(defvar *cv* "https://cv.jake.chvatal.com")
(defvar *phone* "15033308568")

;; example of a flute html template:
(in-package :flute-user)
(element-string (html
              (head
               (link :rel "...")
               (script :src "..."))
              (body
               (div :id "a" :class "b"
                    (p :style "color: red"
                       "Some text")
                    "Some text in div"
                    (img :src "/img/dog.png")
                    (a '(:href "/cat")
                       (img '((:src . "/img/cat.png"))))))))
