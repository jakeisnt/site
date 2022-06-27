(ns site.core
  (:require [hiccup.core :as h]
            [hiccup.page :as hp]
            [hiccup.form :as form])
  (:gen-class))

;; TODO we probably don't need this import
(def style 
  "@import url(\"https://jakeisnt.github.io/styles/main.css\");
  body {
  margin: 0;
  display: flex;
  padding: 2rem;
  flex-direction: column;
  justify-content: center; 
  }")

;; whether to render images or just text!
(def show-imgs nil)


;; TODO we can improve this import...
(def info {:name "Jacob Chvatal"
           :email "jake@isnt.online"
           :twitter "jakeissnt"
           :instagram "jakeisnt"
           :mastodon "jakeisnt"
           :github "jakeisnt"
           :reddit "jakeisnt"
           :phone "15033308568" })

;; play with ascii converters? ditherers? https://github.com/LauJensen/Claskii/blob/master/src/claskii.clj

;; TODO
(defn gen-manifest 
  "Generate a 'site.webmanifest' file with the current information we have.

  Generate the manifest information as a clojure structure,
  then convert it to JSON and serialize it to a file, 
  ensuring that the corresponding JSON structure is correct,"
  []
  "asdf")

;; how can i inject more personality into my website?
;; https://bypaulshen.com/ uses his sketches (& i love the layout)

; ideas:
; - expose .edn file used to generate this (presumably with more info) directly over the web,
;   so we can snag a nicer data representation than html off the internet live
; - when converting to dynamic website, play with andreas gyasin's ideas of moving text on the page;
;   we want the process to be reversible, and we do not want to pollute this page with any more bloat than necessary

(defn gen-homepage 
  "Generate a homepage for the website."
  [info]
  (hp/html5 
    {:lang "en-us"}
    [:style style]
    [:head
     [:title (:name info)]
     [:meta {:charset "utf-8"}]
     [:meta {:property "og:title" :content (:name info)}]
     [:meta {:property "og:type" :content "website"}]


     ;; TODO host current avatar in one place so it can be dynamically loaded. i don't like the idea of using the github avatar - i want to control it myself! updating a new avatar should update some social media profiles to also use that avatar. 
     [:meta {:property "og:image" :content "https://avatars0.githubusercontent.com/u/29869612?s=400&u=32e0c272cbfcc32b8e9585f74ce57d197aa14fb0&v=4"}]

     ;; TODO better description
     [:meta {:name "description" :content "Jacob Chvatal's personal website."}]
     ;; TODO keywords as list, assess keywords, etc.
     [:meta {:name "keywords" :content "jacob, chvatal, webring, programming, languages"}]
     [:meta {:name "author" :content (:name info)}]
     [:meta {:name "robots" :content "follow"}]
     [:meta {:name "viewport" :cntent "width=device-width, initial-scale=1.0"}]
     [:meta {:name "theme-color" :content "#fff"}]

     ;; TODO new icon (make myself?). drop these for now.
     ; [:link {:rel "apple-touch-icon" :sizes "180x180" :href "icons/apple-touch-icon.png"}]
     ; [:link {:rel "icon" :type "image/png" :sizes "32x32" :href "icons/favicon-32x32.png"}]
     ; [:link {:rel "icon" :type "image/png" :sizes "16x16" :href "icons/favicon-16x16.png"}]
     [:link {:rel "manifest" :href "/site.webmanifest"}]]
    [:body
     [:main 
      [:p "Hey, welcome to my site."]

      [:p
       "I'm a student at " [:a {:href "https://northeastern.edu" :target "_blank" :rel "noreferrer"} "Northeastern"] [:br]
       "interested in programming" [:br]
       "languages, user interfaces" [:br]
       "and sustainability."]

      [:p "Feel free to look at " [:a {:href "https://wiki.jacob.chvatal.com"} "my notes"]"," [:br]
        [:a {:href (str "https://github.com/" (:github info))} "my code"] " or " [:a {:href "https://cv.jacob.chvatal.com"} "my CV"] "."]

      [:p "I'm on " [:a {:href (str "https://twitter.com/" (:twitter info))} "twitter"] " and " 
       [:a {:href (str "https://www.instagram.com/" (:instagram info))} "instagram"] "." [:br]
       [:a {:href "mailto:jake@isnt.online" :target "_blank"} "Email"] " or " [:a {:href (str "sms://" (:phone info))} "text"] " me if you'd like."]
       
       ;; "("[:a {:href "./jakeisnt.asc"} "PGP key"]")."

      [:p "Best," [:br] (:name info)]]]

    ;; TODO is this the right way to use the footer? find out!
    [:footer
     [:div {:style "display: flex; flex-direction: row;"}
      [:a {:href "https://creativecommons.org/licenses/by-nc-sa/4.0" :target "_blank" :rel "noreferrer"} "[cc]"]
       ; [:img {:src "icons/cc.svg" :style "margin-right:5px;" :height 30 :width 30 :alt "[cc]"}] ]

      [:a {:href (str "https://merveilles.town/@" (:mastodon info)) :target "_blank" :rel "noreferrer"} "[masto]"]
       ;; [:img {:src "icons/merveilles.svg" :height 30 :width 30 :alt "[merveilles]"}]]

      [:a {:href "https://webring.xxiivv.com/#random" :target "_blank" :rel "noreferrer"} "[ring]"]
       ;;[:img {:src "icons/icon.black.svg" :style "margin-top:-3px;" :height 35 :width 34 :alt "[webring]"}]]
     ]

     [:div {:style "margin-top:1rem;"}
      "This site is optimized for speed." [:br]
      "Alternative experiments to come."]]))

(defn -main
  "Generate a website and print it to STDOUT!"
  [& args]
  (println (gen-homepage info)))
