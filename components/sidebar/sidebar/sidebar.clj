;; -- sidebar

(defn get-page-name
  "Get the name of the article's page.
  Assume it's the first h1 in the file.
  If unavailable, use the filename."
  [md-article target-path]
  (let [backup (file/name target-path)]
    (if (> (count md-article) 2)
      (let [maybe-title (nth md-article 2)]
        (if (= (first maybe-title) :h1)
          (nth maybe-title 2)
          backup))
      backup)))

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
      (collect-folder-paths path-list title)]
     (components/component "toggle-dark-mode" nil nil nil nil)]))

(defn main [file files file-list-idx compiled-html]
  {:depends-on [{:src "/components/sidebar/sidebar.css"}]
   :body (sidebar (:target-path file)
                  (get-page-name compiled-html (:target-path file)))})
