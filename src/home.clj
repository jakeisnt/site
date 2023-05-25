(ns home
  (:require const html components))

;; website home page
(defn home []
  {:depends-on ["lastfm" "neko"]
   :body
   [:html
    (html/head  "/" "Jake Chvatal")
    [:body
     (components/sidebar const/target-dir "~")
     [:div.site-body
      [:main
       [:article.wikipage.aboutMe
        [:p "Hey, I'm Jake Chvatal."]
        [:p  "I'm a software engineer based in Stockholm, Sweden."]
        [:p "During the day, I work at " [:a.external {:href "https://improvin.com"} "Improvin'"]
         ", building tools to help food companies reduce their environmental impact."]

        [:p "On nights and weekends, I " [:a.internal {:href "/pages/index.html"} "write"]
         ", take " [:a.external {:href "https://instagram.com/jakeisnt"} "photos"]
         ", and design simple hardware and software tools."]]

       (components/component "lastfm")]]]]})

(defn html []
  (html/->string (:body (home))))
