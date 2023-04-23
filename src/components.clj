(ns components
  (:require html const))

(defn no-javascript []
  [:noscript
   [:div.git-hist-table
    [:h3 "Thank you for disabling javascript."]
    [:p "This website is augmented with JS, but is perfectly functional without it. The web should be usable with static files alone."]]])

(defn lastfm []
  [:div.lastfm-now-playing-box
   (html/css "/components/lastfm/lastfm.css")
   (html/script "/components/lastfm/lastfm.js")])

(defn toggle-dark-mode []
  [:div.git-hist-table
   [:button.toggle-dark-mode "Change color scheme"]
   [:script {:src "/components/toggle-dark-mode/toggle-dark-mode.js"}]])

(defn link-info-table []
  [:div.git-hist-table
   [:table
    (for [{n :name u :url a :user} const/profiles]
      [:tr [:td n] [:td [:a {:href u} a]]])]])

(defn scroll-up-button []
  [:div.git-hist-table
   [:button.scroll-up-button "Scroll up"]
   (html/script "/components/scroll-up/scroll-up.js")])
