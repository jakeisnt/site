(ns main
  (:require
   [cybermonday.core :as cm]
   [clojure.string :as str]
   [hiccup.core :as h]
   [clojure.java.io :as io]))

(def site-name "Jake Chvatal")
(def source-dir "/home/jake/wiki/pages")
(def target-dir "/home/jake/site/docs/pages")

(defn get-files
  "Get all the files in a directory"
  [dir]
  (let [directory (clojure.java.io/file dir)]
    ;; file-seq includes the dir itself
    (rest (file-seq directory))))

(defn read-file [path]
  (slurp path))

(comment
  (read-file (second (get-files source-dir))))

(defn write-file [content path]
  (spit path content :append false))

(defn parse-md [file-string]
  (:body (cm/parse-md file-string)))

(defn metm [k v]
  [:meta {:name k :content v}])

(defn prop [k v]
  [:meta {:property k :content v}])

(defn split-path [path]
  (str/split path #"/"))

(defn remove-path-prefix [path]
  (str/replace path #"^/home/jake/site/docs/" ""))

(defn collect-folder-paths
  "Collect all the paths to folders in a directory as html."
  ([path-list] (collect-folder-paths path-list "/"))
  ([path-list cur-path]
   (if (empty? (rest path-list))
     (list [:span " / "] [:b (first path-list)])
     (let [fst (first path-list)
           rst (rest path-list)]
       (cons
        [:span " / "]
        (cons
         [:a {:href (str fst "/index.html")} fst]
         (collect-folder-paths rst (str cur-path "/" fst))))))))

(defn sidebar [path]
  (let [path-list (split-path (remove-path-prefix path))]
    [:div.sidebar
     (if (empty? path-list)
       [:b "jake."]
       [:a {:href "/"} "jake."])
     [:a {:href "https://isnt.online"} " ~ "]
     (collect-folder-paths path-list)]))

(defn render-article [md-article target-path]
  (let [page-name "md-article-name"]
    (h/html
     [:html
      [:head
       [:meta {:charset "UTF-8"}]
       [:title (str page-name " | " site-name)] ;; TODO
       (metm "viewport" "width=device-width,initial-scale=1.0")
       (prop "og:title" page-name)
       (prop "og:type" "website")
       (prop "og:url" "https://jake.isnt.online")
       (prop "og:site_name" site-name)
       (metm "description" "hi") ;; TODO
       (metm "keywords" "Operating Systems, webring, programming, languages")
       (metm "author" "Jake Chvatal")
       (metm "robots" "index,follow")
       (metm "theme-color" "#fff") ;; TODO: light mode and dark mode

       ;; TODO favicon
       ;; TODO webmanifest
       [:link {:rel "stylesheet" :href "/style.css"}]
       [:script {:src "/lib.js"}]
       ;; TODO: highlight.js
       ]
      [:body
       [:div.site-body
        (sidebar target-path) ;; TODO
        [:main
         [:article.wikipage
          [:h1.title-top page-name]
          md-article]]
        [:div.git-history-table]        ;; TODO
        ]]])))

(defn change-path [path]
  ;; TODO: Remove hard-coded paths.
  (-> path
      (str/replace ,,, #"/home/jake/wiki/pages" "/home/jake/site/docs/pages")
      (str/replace ,,, #".md" ".html")))

(defn transform-file [path]
  (let [target-path (change-path path)]
    (->
     path
     read-file
     parse-md
     (render-article target-path)
     (write-file (change-path path)))))

(comment (transform-file "/home/jake/wiki/pages/os.md"))

(defn main [a]
  (let [files  (get-files source-dir)]
    (println (second files))
    (doseq [file files]
      (transform-file file))))
