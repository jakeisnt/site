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

(defn neko []
  [:span
   [:div.neko-bed]
   [:div.neko
    (html/script "/components/neko/neko.js")
    (html/css "/components/neko/neko.css")]])

(defn terminal []
  [:div.terminal.git-hist-table
   (html/script "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.4/jquery.js")
   (html/script "https://cdn.jsdelivr.net/gh/jcubic/jquery.terminal@devel/js/jquery.terminal.min.js")
   (html/css "https://cdn.jsdelivr.net/gh/jcubic/jquery.terminal@devel/css/jquery.terminal.min.css")
   (html/script "/components/terminal/terminal.js")
   (html/css "/components/terminal/terminal.css")])

;; -- sidebar
(defn collect-folder-paths
  "Collect all the paths to folders in a directory as html."
  ([path-list title] (collect-folder-paths path-list title nil))
  ([path-list title cur-path]
   (if (empty? (rest path-list))
     (list [:span html/path-delimiter] [:b title])
     (let [fst (first path-list)
           rst (rest path-list)
           cur-path (str (if cur-path cur-path nil) "/" fst)]
       (cons
        [:span html/path-delimiter]
        (cons
         [:a {:href "./index.html"} fst]
         (collect-folder-paths rst title cur-path)))))))

(defn sidebar [path title]
  (let [path-list (html/make-path-list path)]
    [:div.sidebar
     (if (empty? path-list)
       [:b "jake."]
       [:a {:href "/"} "jake."])
     [:a {:href "https://isnt.online"} " ~ "]
     (collect-folder-paths path-list title)
     (html/css "/components/sidebar/sidebar.css")]))
