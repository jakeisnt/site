(ns site.core
  (:require [hiccup.core :as h]
            [hiccup.page :as hp]
            [hiccup.form :as form])
  (:gen-class))

(def style 
  "@import url(\"https://jakeisnt.github.io/styles/main.css\");
  body {
  margin: 0;
  display: flex;
  padding: 20px;
  flex-direction: column;
  justify-content: center; 
  }")

; (defn gen-opengraph []
;   )


(defn gen-page []
  (hp/html5 
    {:lang "en-us"}
    [:style style]
    [:head
     [:title "Jacob Chvatal"]
     [:meta {:charset "utf-8"}]
     [:meta {:property "og:title" :content "Jacob Chvatal"}]
     [:meta {:property "og:type" :content "website"}]
     [:meta {:property "og:image" :content "https://avatars0.githubusercontent.com/u/29869612?s=400&u=32e0c272cbfcc32b8e9585f74ce57d197aa14fb0&v=4"}]
     [:meta {:name "description" :content "Jacob Chvatal's personal website."}]
     [:meta {:name "keywords" :content "jacob, chvatal, webring, programming, languages"}]
     [:meta {:name "author" :content "Jacob Chvatal"}]
     [:meta {:name "robots" :content "follow"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
     [:meta {:name "theme-color" :content "#fff"}]

     [:link {:rel "apple-touch-icon" :sizes "180x180" :href "icons/apple-touch-icon.png"}]
     [:link {:rel "icon" :type "image/png" :sizes "32x32" :href "icons/favicon-32x32.png"}]
     [:link {:rel "icon" :type "image/png" :sizes "16x16" :href "icons/favicon-16x16.png"}]
     [:link {:rel "manifest" :href "/site.webmanifest"}]]
    [:body
     [:main "
Hey, welcome to my site.

I'm a student at " [:a {:href "https://northeastern.edu" :target "_blank" :rel "noreferrer"} "Northeastern"] "
interested in programming 
languages, user interfaces 
and sustainability. 

Feel free to look at " [:a {:href "https://wiki.jacob.chvatal.com"} "my notes"] ", 
"[:a {:href "https://jakeisnt.substack.com/p/coming-soon"} "subscribe"] " to my newsletter,
or check out my " [:a {:href "https://github.com/jakeisnt"} "code"] ".

My CV can be found "[:a {:href "https://cv.jacob.chvatal.com"} "here"]".

Contact me on " [:a {:href "https://twitter.com/jakeissnt"} "twitter"]", 
"[:a {:href "https://reddit.com/user/jakeisnt"} "reddit"]", " [:a {:href "https://www.instagram.com/jakeisnt"} "instagram"]", 
or "[:a {:href "mailto:jake at isnt period online" :target "_blank"} "via email"]" ("[:a {:href "./jakeisnt.asc"} "PGP key"]").

Best, 
Jacob Chvatal

"]]

    [:footer
     [:div {:style "display: flex; flex-direction: row;"}
      [:a {:href "https://creativecommons.org/licenses/by-nc-sa/4.0" :target "_blank" :rel "noreferrer"}
       [:img {:src "icons/cc.svg" :style "margin-right:5px;" :height 30 :width 30 :alt "[cc]"}]]

      [:a {:href "https://merveilles.town/@jakeisnt" :target "_blank" :rel "noreferrer"}
       [:img {:src "icons/merveilles.svg" :height 30 :width 30 :alt "[merveilles]"}]]

      [:a {:href "https://webring.xxiivv.com/#random" :target "_blank" :rel "noreferrer"}
       [:img {:src "icons/icon.black.svg" :style "margin-top:-3px;" :height 35 :width 34 :alt "[webring]"}]]]
     "

This site is optimized for speed.
For more fun, click here.
     "]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (gen-page)))
