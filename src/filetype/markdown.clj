(ns filetype.markdown
  (:require file path html git components
            [cybermonday.core :as cm]))

(defn parse [file-string]
  (:body (cm/parse-md file-string)))

(defn has-title?
  "Does the markdown page have a built-in title"
  [md-article]
  (and
   (> (count md-article) 2)
   (> (count (nth md-article 2)) 2)
   (= (first (nth md-article 2)) :h1)))

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

(defn render-article [md-article file files file-list-idx]
  (let [page-name (get-page-name md-article (:target-path file))
        has-title (has-title? md-article)]
    [:html
     (html/head (:target-path file) page-name)
     [:body
      (components/component "sidebar" file files file-list-idx md-article)
      [:div.site-body
       [:main
        [:article.wikipage
         (if (not has-title) [:h1.title-top page-name] nil)
         md-article]]
       [:div.article-rhs-container
        [:div.article-rhs
         (components/component "page-map" file files file-list-idx md-article)
         (components/component "git-history-table" file files file-list-idx md-article)
         (components/component "prev-next-up-buttons" file files file-list-idx md-article)]]]]]))

(defn contents [file-obj files file-list-idx]
  (let [file-string (file/read (:source-path file-obj))
        md-article (parse file-string)]
    (render-article md-article file-obj files file-list-idx)))
