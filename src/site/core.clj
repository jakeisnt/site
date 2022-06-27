(ns site.core
  (:require [hiccup.core :as h]
            [hiccup.page :as hp]
            [hiccup.form :as form])
  (:gen-class))

;; TODO we probably don't need this import
(def style 
  "html { font-family: sans-serif; }
  body { max-width: 17rem; margin: 0; padding: 1rem 2rem; }")

;; TODO we can improve this import...
(def info {:name "Jake Chvatal"
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
     ;; TODO better description
     [:meta {:name "description" :content "jake chvatal's website"}]
     ;; TODO keywords as list, assess keywords, etc.
     [:meta {:name "keywords" :content "jake, chvatal, webring, programming, languages"}]
     [:meta {:name "author" :content (:name info)}]
     [:meta {:name "robots" :content "follow"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
     [:meta {:name "theme-color" :content "#fff"}]
     [:link {:rel "manifest" :href "/site.webmanifest"}]]
    [:body
     [:main 
      [:p "Hey! I'm Jake, a student at " [:a {:href "https://northeastern.edu" :target "_blank" :rel "noreferrer"} "NEU"]
       " interested in user interfaces, programming languages and sustainability."]

      [:p "I believe that everyone deserves the ability to express themselves with computing. 
          I build better interfaces to today's computers and teach people to take control of their technology."]

      [:p "I also maintain that code should be written to last.
          Ideal software is simple, modular and easily understood.
          It should run on devices twenty years old as easily as on my device today."]

      [:p "I value high impact roles with missions that benefit everyone involved."]

      [:p 
       "At " [:a {:href "https://skira.se"} "Skira"]" I helped democratize "[:br ]" the scandinavian grain market " [:br]
       "and at " [:a {:href "https://theroutingcompany.com"} "TRC"] " provide equitable access to public transportation." [:br]
       "Also " 
       [:a {:href "https://contra.work"} "contra"] " "
       [:a {:href "https://psu.edu"} "psu"] " "
       [:a {:href "https://nixos.org"} "nixos"] " "
       [:a {:href "https://intel.com"} "intel"] " "
       [:a {:href "https://cdkglobal.com"} "cdk"] "."]

      [:p 
       "Here are my " [:a {:href "https://wiki.jacob.chvatal.com"} "notes"]", "
       [:a {:href (str "https://github.com/" (:github info))} "code"] " and " [:a {:href "https://cv.jacob.chvatal.com"} "cv"] "." [:br]

       "I'm on " [:a {:href (str "https://twitter.com/" (:twitter info))} "twitter"] " and " 
       [:a {:href (str "https://www.instagram.com/" (:instagram info))} "instagram"] "." [:br]
       [:a {:href "mailto:jake@isnt.online" :target "_blank"} "Email"] " or " [:a {:href (str "sms://" (:phone info))} "text"] " me if you'd like."]

      [:p "Chat soon," [:br] (:name info)]]
[:div
      [:div {:style "display: flex; flex-direction: row;"}
       [:a {:href "https://creativecommons.org/licenses/by-nc-sa/4.0" :target "_blank" :rel "noreferrer"} "[cc]"]
       [:a {:href "./jakeisnt.asc"} "[pgp]"]
       [:a {:href (str "https://are.na/" (:arena info))} "[are.na]"]]]]))

(defn -main
  "Generate a website and print it to STDOUT!"
  [& args]
  (println (gen-homepage info)))
