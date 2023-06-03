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
(defn component [component-name file files file-list-idx]
  (let [component-function (load-file (str "/home/jake/site/components/" component-name "/" component-name ".clj"))
        cfg (component-function file files file-list-idx)
        deps (:depends-on cfg)
        body (:body cfg)]
    [:span body (make-deps deps)]))

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
     (component "toggle-dark-mode" nil nil nil)]))

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
