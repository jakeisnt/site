(load "~/quicklisp/setup.lisp")
(load "./util.lisp")

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


(defun checkbox-menu ()
  (spinneret::with-html
    (:div :style "margin-top: 1rem; padding: 0.5rem 0.25rem; max-width: 10rem; border: 1px solid black; background-color: hsl(0, 0%, 96.5%);"
          (:input :type "checkbox" :id "hypothesis-checkbox" :checked "false" :onclick (parenscript:ps (toggle-hypothesis)) "hypothes.is"))))

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
       (:p "Hey! I'm Jake, a student at " (:a :href "https://northeastern.edu" :target "_blank" :rel "noreferrer" "Northeastern")
          " interested in user interfaces, programming languages and sustainability.")

       (::p "I believe that everyone deserves the ability to use computing to express themselves.
          To this end, I aim to build better interfaces - from web UIs to " (:a :href "https://github.com/jakeisnt/nixcfg/" "unix service layers") " - that improve ways in which people understand and interact with their computers.")

       (:p "I value high impact roles at organizations with missions that directly benefit people.")

       (:p
        "At " (:a :href "https://skira.se" "Skira") ", I helped democratize " (:br) " the Scandinavian grain market, " (:br)
        "and at " (:a :href "https://theroutingcompany.com" "TRC") " I helped provide equitable access to transporation worldwide."

        " Other experience includes "
        (:a :href "https://sandboxnu.com" "sbox") ", "
        (:a :href "https://psu.edu" "psu") ", "
        (:a :href "https://intel.com" "intel") ", "
        (:a :href "https://cdkglobal.com" "cdk") "." (:br)
        "More details on " (:a :href *cv* "my full CV") ".")

       (:p
        "Check out my " (:a :href "https://wiki.jacob.chvatal.com" "notes") " and "
        (:a :href (concatenate 'string "https://github.com/" *github*) "code") "!" (:br))

       (:p "I'm on " (:a :href (concatenate 'string "https://twitter.com/" *twitter*) "twitter") " and "
          (:a :href (concatenate 'string "https://www.instagram.com/" *instagram*) "instagram") "." (:br)
          (:a :href "mailto:jake@isnt.online" :target "_blank" "Email") " or "(:a :href (concatenate 'string "sms://" *phone*) "text")  " me if you'd like.")

       (:p "Chat soon," (:br) *name*)
       (:div
        (:div :style "display: flex; flex-direction: row;"
             (:a :class "foot" :href "https://creativecommons.org/licenses/by-nc-sa/4.0" :target "_blank" :rel "noreferrer" "[cc]")
             (:a :class "foot" :href "./jakeisnt.asc" "[pgp]")
             (:a :class "foot" :href (concatenate 'string "https://are.na/" *arena*) "[are.na]")))

       (checkbox-menu))

      ;; load scripts at the end
      (:script :type "text/javascript" :src "./lib.js" "")))))
