(ns html
  (:require
   [const :as const]
   [hiccup.core :as h]
   [clojure.string :as str]))

;; --- general ---

(defn metm [k v]
  [:meta {:name k :content v}])

(defn prop [k v]
  [:meta {:property k :content v}])

(defn head
  "A page header that works for all pages"
  [title]
  [:head
   [:meta {:charset "UTF-8"}]
   [:title (str title " | " const/site-name)]
   (metm "viewport" "width=device-width,initial-scale=1.0")
   (prop "og:title" title)
   (prop "og:type" "website")
   (prop "og:url" const/target-url)
   (prop "og:site_name" const/site-name)
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
   ])

(defn ->string
  "Serialize Hiccup-compatible data to a string"
  [hiccup-struct]
  (str "<!DOCTYPE html>" (h/html hiccup-struct)))

;; --- sidebar

(defn split-path [path]
  ;; splitting on the first /" gives us a front empty string that we drop
  (rest (str/split path #"/")))

(defn remove-path-prefix [path]
  (str/replace path (re-pattern (str "^" const/target-dir)) ""))

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
