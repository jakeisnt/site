(ns site.core
  (:require [hiccup.core :as h]
            [hiccup.page :as hp]
            [hiccup.form :as form])
  (:gen-class))

;; TODO we probably don't need this import
(def style 
  "
  html { font-family: sans-serif; }
  body { margin: 0; display: flex; padding-left: 2rem; padding-top: 1rem; flex-direction: column; justify-content: center; }
  main { max-width: 17rem; }
  ")

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
           :arena "jake-isnt"
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

     ;; TODO avatar? i don't like this one, really.
     ;; [:meta {:property "og:image" :content "https://avatars0.githubusercontent.com/u/29869612?s=400&u=32e0c272cbfcc32b8e9585f74ce57d197aa14fb0&v=4"}]

     ;; TODO better description
     [:meta {:name "description" :content "hi im jake chvatal"}]
     ;; TODO keywords as list, assess keywords, etc.
     [:meta {:name "keywords" :content "jacob, chvatal, webring, programming, languages"}]
     [:meta {:name "author" :content (:name info)}]
     [:meta {:name "robots" :content "follow"}]
     [:meta {:name "viewport" :cntent "width=device-width, initial-scale=1.0"}]
     [:meta {:name "theme-color" :content "#fff"}]
     [:link {:rel "manifest" :href "/site.webmanifest"}]]
    [:body
     [:main 
      [:p "Hey, welcome to my site."]

      ; [:p
      ;  "I'm a student at " [:a {:href "https://northeastern.edu" :target "_blank" :rel "noreferrer"} "Northeastern"] [:br]
      ;  "interested in programming languages, design systems, user interfaces and sustainability."]

      [:p {:open true}
       ;;[:summary "mission"]
       [:span "I believe that everyone deserves the ability to express themselves with technology. 
              I work to build better interfaces to today's computers and teach people to take control of their technology.
              "]]

      [:p "I believe that software should be built to last.
          Software should be modular, disciplined, and easy to understand.
          It should run on devices twenty years old just as easily as on my device today."]

      [:p {:open true}
       ;; [:summary "work"]
       [:span 
        "I cherish high impact roles that benefit everyone involved." [:br]
        "At " [:a {:href "https://skira.se"} "Skira"]" I helped revolutionize the scandinavian grain industry " [:br]
        "and at " [:a {:href "https://theroutingcompany.com"} "TRC"] " provided equitable access to public transportation." [:br]
        "Also " 
          [:a {:href "https://contra.work"} "contra"] " "
          [:a {:href "https://psu.edu"} "psu"] " "
          [:a {:href "https://nixos.org"} "nixos"] " "
          [:a {:href "https://intel.com"} "intel"] " "
          [:a {:href "https://cdkglobal.com"} "cdk"] "."]]

      [:p {:open true}
       ;;[:summary "contact"]
       [:span "Here are my " [:a {:href "https://wiki.jacob.chvatal.com"} "notes"]", "
        [:a {:href (str "https://github.com/" (:github info))} "code"] " and " [:a {:href "https://cv.jacob.chvatal.com"} "cv"] "." [:br]

        "I'm on " [:a {:href (str "https://twitter.com/" (:twitter info))} "twitter"] " and " 
        [:a {:href (str "https://www.instagram.com/" (:instagram info))} "instagram"] "." [:br]
        [:a {:href "mailto:jake@isnt.online" :target "_blank"} "Email"] " or " [:a {:href (str "sms://" (:phone info))} "text"] " me if you'd like."]]

      [:p "Chat soon," [:br] (:name info)]]]

    ;; TODO is this the right way to use the footer? find out!
    [:footer
     [:div {:style "display: flex; flex-direction: row;"}
      [:a {:href "https://creativecommons.org/licenses/by-nc-sa/4.0" :target "_blank" :rel "noreferrer"} "[cc]"]
      ; [:img {:src "icons/cc.svg" :style "margin-right:5px;" :height 30 :width 30 :alt "[cc]"}] ]

      ;; [:a {:href (str "https://merveilles.town/@" (:mastodon info)) :target "_blank" :rel "noreferrer"} "[masto]"]
      ;; [:img {:src "icons/merveilles.svg" :height 30 :width 30 :alt "[merveilles]"}]]

      [:a {:href "./jakeisnt.asc"} "[pgp]"]
      [:a {:href (str "https://are.na/" (:arena info))} "[are.na]"]

      ;; [:a {:href "https://webring.xxiivv.com/#random" :target "_blank" :rel "noreferrer"} "[ring]"]
      ;;[:img {:src "icons/icon.black.svg" :style "margin-top:-3px;" :height 35 :width 34 :alt "[webring]"}]]
      ]

     [:div {:style "margin-top:1rem;"}
      "This site is optimized for speed." [:br]
      "Alternative experiments to come."]]))

(defn -main
  "Generate a website and print it to STDOUT!"
  [& args]
  (println (gen-homepage info)))
