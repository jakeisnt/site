(ns home
  (:require const html components))

;; website home page
(defn html []
  (html/->string
   [:html
    (html/head  "/" "Jake Chvatal")
    [:body
     (components/sidebar "/" "index")
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

       (components/no-javascript)
       (components/link-info-table)
       (components/component "lastfm")
       (components/component "neko")
       ;; (components/terminal)
       ]]]]))
