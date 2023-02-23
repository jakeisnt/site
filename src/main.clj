(ns main
  (:require
   [cybermonday.core :as cm]
   [clojure.string :as str]
   [hiccup.core :as h]
   [file :as file]
   [git :as git]))

(def site-name "Jake Chvatal")

(def source-url "https://github.com/jakeisnt/wiki")
(def source-dir "/home/jake/wiki")
(def pages-source-dir (str source-dir "/pages"))

(def target-url "https://jake.isnt.online")
(def target-dir "/home/jake/site/docs")
(def pages-target-dir (str target-dir "/pages"))

;; TODO
;; to get up to par with the common lisp build:
;; - home page reimplementation
;; - parser for scripts/
;; - index page for folders

;; To improve:
;; - Journals page

(defn swap-extension [file]
  (str/replace file #"\.html$" ".md"))

(comment
  (file/read (second (file/list pages-source-dir))))

(defn parse-md [file-string]
  (:body (cm/parse-md file-string)))

(defn metm [k v]
  [:meta {:name k :content v}])

(defn prop [k v]
  [:meta {:property k :content v}])

(defn split-path [path]
  ;; splitting on the first "/" gives us a front empty string that we drop
  (rest (str/split path #"/")))

(defn remove-path-prefix [path]
  (str/replace path (re-pattern (str "^" target-dir)) ""))

(defn collect-folder-paths
  "Collect all the paths to folders in a directory as html."
  ([path-list title] (collect-folder-paths path-list title nil))
  ([path-list title cur-path]
   (if (empty? (rest path-list))
     (list [:span " / "] [:b title])
     (let [fst (first path-list)
           rst (rest path-list)
           cur-path (str (when cur-path cur-path) "/" fst)]
       (cons
        [:span " / "]
        (cons
         [:a {:href "./index.html"} fst]
         (collect-folder-paths rst title cur-path)))))))

(defn sidebar [path title]
  (let [path-list (split-path (remove-path-prefix path))]
    [:div.sidebar
     (if (empty? path-list)
       [:b "jake."]
       [:a {:href "/"} "jake."])
     [:a {:href "https://isnt.online"} " ~ "]
     (collect-folder-paths path-list title)]))

(defn history-link [long-hash file-path]
  (str source-url "/blob/" long-hash "/" file-path))

(defn git-history-table [file-path]
  (let [file-path (swap-extension (remove-path-prefix file-path))
        git-log (git/log file-path source-dir)]
    [:div.git-hist-table
     [:table
      [:tbody
       (for [commit git-log]
         [:tr
          [:td (:commit-date commit)]
          [:td [:a {:href (history-link (:long-hash commit) (:file-path commit))} (:short-hash commit)]]])]]]))

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

(defn has-title [md-article]
  (and
   (> (count md-article) 2)
   (> (count (nth md-article 2)) 2)
   (= (first (nth md-article 2)) :h1)))

(defn render-article [md-article target-path]
  (let [page-name (get-page-name md-article target-path)
        has-title (has-title md-article)]
    (h/html
     [:html
      [:head
       [:meta {:charset "UTF-8"}]
       [:title (str page-name " | " site-name)]
       (metm "viewport" "width=device-width,initial-scale=1.0")
       (prop "og:title" page-name)
       (prop "og:type" "website")
       (prop "og:url" target-url)
       (prop "og:site_name" site-name)
       (metm "description" "hi") ;; TODO
       ;; TODO pull some of these from the articles
       (metm "keywords" "Operating Systems, webring, programming, languages")
       (metm "author" "Jake Chvatal")
       (metm "robots" "index,follow")
       (metm "theme-color" "#fff") ;; TODO: light mode and dark mode?

       ;; TODO favicon
       ;; TODO webmanifest
       [:link {:rel "stylesheet" :href "/style.css"}]
       [:script {:src "/lib.js"}]
       ;; TODO: highlight.js
       ]
      [:body
       [:div.site-body
        (sidebar target-path page-name)
        [:main
         [:article.wikipage
          (when (not has-title) [:h1.title-top page-name])
          md-article]]
        (git-history-table target-path)]]])))

(defn change-path [path]
  (-> path
      (str/replace (re-pattern pages-source-dir) pages-target-dir)
      (str/replace #".md" ".html")))

(defn transform-file [path]
  (let [target-path (change-path path)]
    (->
     path
     file/read
     parse-md
     (render-article target-path)
     (file/write target-path))))

(defn -main [_]
  (let [files (file/list pages-source-dir)]
    (doseq [file files]
      (transform-file file))))
