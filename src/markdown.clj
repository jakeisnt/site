(ns markdown
  (:require
   file
   html
   git
   components
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

(defn render-article [md-article source-path target-path files file-list-idx]
  (let [page-name (get-page-name md-article target-path)
        has-title (has-title? md-article)]
    (html/->string
     [:html
      (html/head target-path page-name)
      [:body
       (components/sidebar target-path page-name)
       [:div.site-body
        [:main
         [:article.wikipage
          (if (not has-title) [:h1.title-top page-name] nil)
          md-article]]
        [:div.article-rhs-container
         [:div.article-rhs
          (components/page-map md-article target-path)
          (components/git-history-table source-path)
          (components/prev-next-up-buttons files file-list-idx)]]]]])))

(defn ->file
  "Transform a file from source to target."
  [source-path target-path files file-list-idx]
  (->
   source-path
   file/read
   parse
   (render-article source-path target-path files file-list-idx)
   (file/write target-path)))
