(ns home
  (:require const html))

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
   [:script "runOnDesktop(getNowPlaying);"]])

(defn link-info-table []
  [:div.git-hist-table
   [:table
    (for [{n :name u :url a :user} const/profiles]
      [:tr [:td n] [:td [:a {:href u} a]]])]])

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
