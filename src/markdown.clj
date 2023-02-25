(ns markdown
  (:require
   [path :as path]
   [file :as file]
   [html :as html]
   [cybermonday.core :as cm]
   [git :as git]))

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

(defn render-article [md-article source-path target-path]
  (let [page-name (get-page-name md-article target-path)
        has-title (has-title? md-article)]
    (html/->string
     [:html
      (html/head page-name)
      [:body
       [:div.site-body
        (html/sidebar target-path page-name)
        [:main
         [:article.wikipage
          (when (not has-title) [:h1.title-top page-name])
          md-article]]
        (git/history-table source-path)]]])))

(defn ->file
  "Transform a file from source to target."
  [source-path source-dir target-dir]
  (let [target-path (path/source->target source-path source-dir target-dir)]
    (->
     source-path
     file/read
     parse
     (render-article source-path target-path)
     (file/write target-path))))
