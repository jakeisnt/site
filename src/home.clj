(ns home
  (:require
   [html :as html]))

;; website home page

(defn no-js []
  [:noscript
   [:div.git-hist-table
    [:h3 "Thank you for disabling javascript."]
    [:p "This website is augmented with JS, but is perfectly functional without it. The web should be usable with static files alone."]]])

(defn lastfm-now-playing []
  [:span
   [:link {:rel "stylesheet" :href "/lastfm.css"}]
   [:script {:src "/lfmNowPlaying.js"}]
   [:script "getNowPlaying();"]])

(defn link-info-table []
  [:table
   [:tr [:td "Mastodon"] [:td [:a {:href "https://merveilles.town/@jakeisnt"} "jakeisnt"]]]
   [:tr [:td "Twitter"] [:td [:a {:href "https://twitter.com/jakeissnt"} "jakeissnt"]]]
   [:tr [:td "GitHub"] [:td [:a {:href "https://github.com/jakeisnt"} "jakeisnt"]]]
   [:tr [:td "Email"] [:td [:a {:href "mailto://jake+website@isnt.online"} "jake @ ~"]]]])

(defn html []
  (html/->string
   [:html
    (html/head "Jake Chvatal")
    [:body
     [:div.site-body
      (html/sidebar "/" "index")
      [:main
       [:article.wikipage
        [:p "Hey, I'm Jake."]
        [:p "I like old tools and simple ideas."]
        [:p "Check out my " [:a.external {:href "/pages/index.html"} "notes"] " for more."]]
       (no-js)
       (lastfm-now-playing)
       (link-info-table)]]]]))
