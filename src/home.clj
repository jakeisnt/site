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
       [:article.wikipage.aboutMe
        [:p "Hey, I'm Jake."]
        [:p  "I'm a Stockholm-based software engineer."]
        [:p "During the day, I work at " [:a.external {:href "https://improvin.com"} "Improvin'"]
         ", building tools to help food companies reduce their environmental impact."]

        [:p "On nights and weekends, I " [:a.internal {:href "/pages/index.html"} "write"]
         ", take " [:a.external {:href "https://instagram.com/jakeisnt"} "photos"]
         ", and design small software tools."]

        ;; [:p "I value ideas that are simple, quick, and robust. Tools - the things people own and the programs they use - should allow their users to express themselves, and should be made to be passed on to your friend or child."]
        ]

       (no-js)
       (lastfm-now-playing)
       (link-info-table)]]]]))
