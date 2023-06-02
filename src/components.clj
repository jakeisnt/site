(ns components
  (:require
   html const file path git
   [clojure.core.match :refer [match]]))

;; compile dependencies based on type
(defn make-deps [deps]
  (for [dep deps]
    (match (file/extension (:src dep))
      "js" (html/defer-script (:src dep))
      "css" (html/css (:src dep))
      "scss" (html/css (path/replace-extension (:src dep) "css")))))

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

;; git history table
(defn git-history-table
  "Renders the git history for a file given its path."
  [source-path source-dir]
  (let [git-log (git/log (file/path source-path) source-dir)]
    [:div.git-history-table-container
     [:span.git-history-table-title "Revisions"]
     [:table.git-history-table
      [:tr
       [:th "Date"]
       [:th "Hash"]]
      [:tbody
       (for [commit git-log]
         [:tr
          [:td.commit-date-tr (:commit-date commit)]
          [:td.commit-link-tr [:a {:href (git/history-link
                                          (:long-hash commit)
                                          (path/remove-prefix (:file-path commit) source-dir))} (:short-hash commit)]]])]]]))

;; generate a map of a page with links to content
(defn page-map [page path]
  (let [tags (html/find-tags page :h1 :h2 :h3 :h4 :h5 :h6)
        path-link (path/remove-prefix path const/target-dir)]
    [:div.sitemap-container
     [:span.sitemap-title "In this article"]
     [:table.sitemap
      (map (fn [tag]
             (let [tag-id (:id (second tag))
                   tag-text (or
                             (and (> 2 (count tag)) (nth tag 2))
                             tag-id)]
               [:tr
                [:td {:id (str "page-map-" tag-id)} [:a {:href (str path-link "#" tag-id)} tag-text]]]))
           tags)]]))

;; tor snowflake - let others proxy
(defn snowflake-tool []
  [:iframe {:src "https://snowflake.torproject.org/embed.html" :width 320 :height 240 :frameborder 0 :scrolling "no"}])

(defn prev-next-up-buttons  [file files file-list-idx]
  (let [prev-file (and (> file-list-idx 0)
                       (nth files (dec file-list-idx)))
        next-file (and (< file-list-idx (dec (count files)))
                       (nth files (inc file-list-idx)))
        up-link (path/folder (:link file))]
    [:div.prev-next-up-buttons-container
     "Navigation"
     [:table.prev-next-up-buttons
      (when prev-file
        [:tr
         [:td "Previous"]
         [:td [:a.prev-button {:href (:link prev-file)}  (:name prev-file)]]])
      (when next-file
        [:tr
         [:td "Next"]
         [:td [:a.next-button {:href (:link next-file)}  (:name next-file)]]])
      [:tr
       [:td "Up"]
       [:td [:a.up-button {:href up-link} (file/title up-link)]]]]]))
