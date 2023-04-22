(ns home
  (:require const html))

;; website home page

(defn no-js []
  [:noscript
   [:div.git-hist-table
    [:h3 "Thank you for disabling javascript."]
    [:p "This website is augmented with JS, but is perfectly functional without it. The web should be usable with static files alone."]]])

(defn lastfm-now-playing []
  [:div.lastfm-now-playing-box
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
    (html/head  "/" "Jake Chvatal")
    [:body
     [:div.site-body
      (html/sidebar "/" "index")
      [:main
       [:article.wikipage.aboutMe
        [:p "Hey, I'm Jake Chvatal."]
        [:p  "I'm a software engineer based in Stockholm, Sweden."]
        [:p "During the day, I work at " [:a.external {:href "https://improvin.com"} "Improvin'"]
         ", building tools to help food companies reduce their environmental impact."]

        [:p "On nights and weekends, I " [:a.internal {:href "/pages/index.html"} "write"]
         ", take " [:a.external {:href "https://instagram.com/jakeisnt"} "photos"]
         ", and design simple hardware and software tools."]]

       (no-js)
       (link-info-table)
       (lastfm-now-playing)]]]]))
