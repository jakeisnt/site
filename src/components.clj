(ns components
  (:require
   html const file
   [clojure.core.match :refer [match]]))

;; compile dependencies based on type
(defn make-deps [deps]
  (for [dep deps]
    (match (:type dep)
      :js (html/defer-script (:src dep))
      :css (html/css (:src dep)))))

;; imports a component and its dependencies where called
(defn component [component-name]
  (let [cfg (file/load-edn (str "/home/jake/site/components/" component-name "/" component-name ".edn"))
        deps (:depends-on cfg)
        body (:body cfg)]
    [:span body (make-deps deps)]))

(defn no-javascript []
  [:noscript
   [:div.git-hist-table
    [:h3 "Thank you for disabling javascript."]
    [:p "This website is augmented with JS, but is perfectly functional without it. The web should be usable with static files alone."]]])

(defn link-info-table []
  [:div.git-hist-table
   [:table
    (for [{n :name u :url a :user} const/profiles]
      [:tr [:td n] [:td [:a {:href u} a]]])]])

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

;;; path: absolute path to the sidebar (e.g. /home/jake/...)
(defn sidebar [path title]
  (let [path-list (html/make-path-list path)]
    [:div.sidebar
     [:div.url-path
      (if (empty? path-list)
        [:b "jake."]
        [:a {:href "/"} "jake."])
      [:a {:href "https://isnt.online"} " ~ "]
      (collect-folder-paths path-list title)
      (html/css "/components/sidebar/sidebar.css")]
     (component "toggle-dark-mode")]))

;; generate a map of a page with links to content
(defn page-map [page url]
  (let [tags (html/find-tags page :h1 :h2 :h3 :h4 :h5 :h6)]
    [:div.sitemap-container
     [:table.sitemap
      (map (fn [tag]
             (let [tag-id (:id (second tag))
                   tag-text (nth tag 2)]
               [:tr
                [:td [:a {:href (str url "#" tag-id)} tag-text]]]))
           tags)]]))
