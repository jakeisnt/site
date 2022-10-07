(load "~/quicklisp/setup.lisp")
(load "./src/util.lisp")
(load "./src/components.lisp")

(ql:quickload :spinneret)
(ql:quickload 'css-lite)
(ql:quickload :parenscript)

(defpackage homepage
  (:use :cl))

;; https://hg.stevelosh.com/stevelosh.com/file/tip/generate.lisp is legendary

;; https://gist.github.com/privet-kitty/e5abd3f09485c5828af81ad02e8c2baf
;; worth looking into;;

;; writing javascript code with common lisp: https://40ants.com/lisp-project-of-the-day/2020/05/0071-parenscript.html
;; how do we make a nice way of interactively compiling it? is there a good way to set up a dev server to do this kind of thing?

;; TODO: make a macro to reduce noise here
;; (defmacro defparameters)
;; (defmacro defv (name val)
;;   `(defparameter ,(read-from-string (format nil "~{~a~}" name)) ,val))

;; (defv name "jake")

(in-package :homepage)


(defparameter *name* "Jake Chvatal")
(defparameter *email* "jake*site@isnt.online")
(defparameter *twitter* "@jakeissnt")
(defparameter *instagram* "jakeisnt")
(defparameter *mastodon* "jakeisnt")
(defparameter *github* "jakeisnt")
(defparameter *reddit* "jakeisnt")
(defparameter *arena* "jake-isnt")
(defparameter *cv* "https://cv.jake.chvatal.com")
(defparameter *phone* "15033308568")


;; idea: route editor!
;; sidebar allows you to interactively add parts to your url
;; press some '+' button
;; adds a slash and a box
;; shows a list of all the possible things taht could go there
;; you type and search or just click on one of them and you're taken to that route,

;; i would love to actually have this in the browser,
;; but for now this is fine

;; also, for the parser to work properly with this page,
;; we might want to group some things into paragraphs
;; we might also want to see what we can do about that url thing on mobile;
;; maybe it's a good idea to move it to the top of the screen and fix it there

;; also: the name of the website should reflect the path. how?
;; it shouldn't be literal, but it should be aesthetically similar.

(defun homepage ()
  (spinneret::with-html
      (:html
       :lang "en-us"
       (:head
        (:title *name*)
        (:link :rel "stylesheet" :href "./style.css")
        (:meta :charset "utf-8")
        (:meta :property "og:title" :content *name*)
        (:meta :property "og:type" :content "website")
        (:meta :name "description" :content "jake chvatal's website")
        (:meta :name "keywords" :content "jake, chvatal, webring, programming, languages")
        (:meta :name "author" :content *name*)
        (:meta :name "robots" :content "follow")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
        (:meta :name "theme-color" :content "#fff")
        (:link :rel "manifest" :href "site.webmanifest"))
       (:body
        (:main
         (:p "Hey!")
         (:p (:span "I'm Jake, a student at ")
             (:a :href "https://northeastern.edu" :target "_blank" :rel "noreferrer" "Northeastern")
             (:span " interested in user interfaces, programming languages and sustainability."))

         (:p (:span "I believe that everyone deserves the ability to use computing to express themselves.
          To this end, I aim to build better interfaces - from web UIs to ") (:a :href "https://github.com/jakeisnt/nixcfg/" "unix service layers")
          (:span " - that improve ways in which people understand and interact with their computers."))

         (:p (:span "I value high impact roles at organizations with missions that directly benefit people. ")
             (:span "At ")
             (:a :href "https://skira.se" "Skira")
             (:span " I helped democratize the Scandinavian grain market ")
             (:span "and at ")
             (:a :href "https://theroutingcompany.com" "TRC")
             (:span " I helped provide equitable access to public transporation. For the detailed rundown, you can check out ")
             (:a :href *cv* "my CV")
             (:span "."))
         (:p
          (:span "Check out my ")
          (:a :href "https://wiki.jacob.chvatal.com" "notes")
          (:span " and ")
          (:a :href (concatenate 'string "https://github.com/" *github*) "code")
          (:span "!")
          (:span "I'm on ")
          (:a :href (concatenate 'string "https://twitter.com/" *twitter*) "twitter")
          (:span " and ")
          (:a :href (concatenate 'string "https://www.instagram.com/" *instagram*) "instagram")
          (:span ".")
          (:a :href "mailto:jake@isnt.online" :target "_blank" "Email")
          (:span " or ")
          (:a :href (concatenate 'string "sms://" *phone*) "text")
          (:span " me if you'd like."))

         (:p "Chat soon," (:br) *name*)
         (:div :style "display: flex; flex-direction: row;"
               (:a :class "foot" :href "https://creativecommons.org/licenses/by-nc-sa/4.0" :target "_blank" :rel "noreferrer" "[cc]")
               (:a :class "foot" :href "./jakeisnt.asc" "[pgp]")
               (:a :class "foot" :href (concatenate 'string "https://are.na/" *arena*) "[are.na]"))

         ;; Components: load them in the body
         (comp::checkbox-menu)
         (comp::sidebar))

        ;; load scripts at the end
        (:script :type "text/javascript" :src "./lib.js" "")))))
