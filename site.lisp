(load "~/quicklisp/setup.lisp")

(ql:quickload :flute)
(ql:quickload :flute-test)

(defpackage flute-user
  (:use :cl :flute))

;; https://gist.github.com/privet-kitty/e5abd3f09485c5828af81ad02e8c2baf
;; worth looking into;;


;; TODO: make a macro to reduce noise here
;; (defmacro defvars)
;; (defmacro defv (name val)
;;   `(defvar ,(read-from-string (format nil "~{~a~}" name)) ,val))

;; (defv name "jake")

(defvar *style* "
html { font-size: 18px; letter-spacing: 1px; font-family: \"Helvetica Neue\", Arial, sans-serif; word-wrap: break-word; hyphens: auto; }
body { max-width: 16rem; margin: 0; padding: 1rem 2rem; }
a { margin-right: -5px; }
.foot { padding-right: 0.5rem; }
")

(defvar *name* "Jake Chvatal")
(defvar *email* "jake*site@isnt.online")
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


(defun gen-homepage ()
  (html
   :lang "en-us"
   (head
    (title *name*)
    (style +style+)
    (meta :charset "utf-8")
    (meta :property "og:title" :content *name*)
    (meta :property "og:type" :content "website")
    (meta :name "description" :content "jake chvatal's website")
    (meta :name "keywords" :content "jake, chvatal, webring, programming, languages")
    (meta :name "author" :content *name*)
    (meta :name "robots" :content "follow")
    (meta :name "viewport" :content "width=device-width, initial-scale=1.0")
    (meta :name "theme-color" :content "#fff")
    (link :rel "manifest" :href "site.webmanifest"))
   (body
    (main
     (p "Hey! I'm Jake, a student at " (a :href "https://northeastern.edu" :target "_blank" :rel "noreferrer" "Northeastern")
        " interested in user interfaces, programming languages and sustainability.")

     (p "I believe that everyone deserves the ability to use computing to express themselves.
          To this end, I aim to build better interfaces - from web UIs to " (a :href "https://github.com/jakeisnt/nixcfg/" "unix service layers") " - that improve ways in which people understand and interact with their computers.")

     (p "I value high impact roles at organizations with missions that directly benefit people.")

     (p
      "At " (a :href "https://skira.se" "Skira") ", I helped democratize " (br) " the Scandinavian grain market, " (br)
      "and at " (a :href "https://theroutingcompany.com" "TRC") " I provide equitable access to public transportation." (br)
      "Others: "
      (a :href "https://sandboxnu.com" "sbox") ", "
      (a :href "https://psu.edu" "psu") ", "
      (a :href "https://intel.com" "intel") ", "
      (a :href "https://cdkglobal.com" "cdk") "." (br)
      "More details on " (a :href *cv* "my full CV") ".")

     (p
      "Check out my " (a :href "https://wiki.jacob.chvatal.com" "notes") " and "
      (a :href (concatenate 'string "https://github.com/" *github*) "code") "!" (br))

     (p "I'm on " (a :href (concatenate 'string "https://twitter.com/" *twitter*) "twitter") " and "
        (a :href (concatenate 'string "https://www.instagram.com/" *instagram*) "instagram") "." (br)
        (a :href "mailto:jake@isnt.online" :target "_blank" "Email") " or "(a :href (concatenate 'string "sms://" *phone*) "text")  " me if you'd like.")

     (p "Chat soon," (br) *name*)
     (div
      (div :style "display: flex; flex-direction: row;")
      (a :class "foot" :href "https://creativecommons.org/licenses/by-nc-sa/4.0" :target "_blank" :rel "noreferrer" "[cc]")
      (a :class "foot" :href "./jakeisnt.asc" "[pgp]")
      (a :class "foot" :href (concatenate 'string "https://are.na" *arena*) "[are.na]"))))))


(gen-homepage)
