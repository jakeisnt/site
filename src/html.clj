(ns html
  (:require
   [const :as const]
   [hiccup.core :as h]
   [clojure.string :as str]))

;; --- general ---

;; delimits paths in title and in body
(def ^:const path-delimiter " / ")

(defn metm [k v]
  [:meta {:name k :content v}])

(defn prop [k v]
  [:meta {:property k :content v}])

(defn script
  ([src] [:script {:src src :id src}])
  ([src body] [:script {:src src :id src} body]))

(defn css
  ([href] [:link {:rel "stylesheet" :type "text/css" :href href :id href}])
  ([href body] [:link {:rel "stylesheet" :type "text/css" :href href :id href} body]))

(defmacro favicon []
  [:link {:rel "icon" :type "image/x-icon" :href "/favicon/favicon.ico"}]
  [:link {:rel "apple-touch-icon" :href "/favicon/apple-touch-icon.png"}])

(defn split-path [path]
  ;; splitting on the first /" gives us a front empty string that we drop
  (rest (str/split path #"/")))

(defn remove-path-prefix [path]
  (str/replace path (re-pattern (str "^" const/target-dir)) ""))

(defn make-path-list [path]
  (split-path (remove-path-prefix path)))

(defn collect-folder-paths-string [path-list title]
  (if (empty? (rest path-list))
    title
    (str (collect-folder-paths-string (rest path-list) title) path-delimiter (first path-list))))

(defn head
  "A page header that works for all pages"
  [path title]
  [:head
   [:meta {:charset "UTF-8"}]
   [:title (str (collect-folder-paths-string (make-path-list path) title) path-delimiter const/site-name)]
   (metm "viewport" "width=device-width,initial-scale=1.0")
   (prop "og:title" title)
   (prop "og:type" "website")
   (prop "og:url" const/target-url)
   (prop "og:site_name" const/site-name)
   (metm "description" "hi") ;; TODO
   ;; TODO pull this information from the articles
   (metm "keywords" "Operating Systems, webring, programming, languages")
   (metm "author" "Jake Chvatal")
   (metm "robots" "index,follow")

   ;; TODO: keep this synced with global theme somehow?
   [:meta  {:name "theme-color" :media "(prefers-color-scheme: light)" :content "white"}]
   [:meta {:name "theme-color" :media "(prefers-color-scheme: dark)" :content "#111"}]
   (favicon)

   [:link {:rel "manifest" :href "/manifest.json"}]

   (css "/style.css")
   (css "/elements.css")
   (css "/global.css")
   (script "/lib.js")
   ;; TODO: highlight.js
   ])

(defn ->string
  "Serialize Hiccup-compatible data to a string"
  [hiccup-struct]
  (str "<!DOCTYPE html>" (h/html hiccup-struct)))

;; --- sidebar

(defn collect-folder-paths
  "Collect all the paths to folders in a directory as html."
  ([path-list title] (collect-folder-paths path-list title nil))
  ([path-list title cur-path]
   (if (empty? (rest path-list))
     (list [:span path-delimiter] [:b title])
     (let [fst (first path-list)
           rst (rest path-list)
           cur-path (str (if cur-path cur-path nil) "/" fst)]
       (cons
        [:span path-delimiter]
        (cons
         [:a {:href "./index.html"} fst]
         (collect-folder-paths rst title cur-path)))))))

(defn sidebar [path title]
  (let [path-list (make-path-list path)]
    [:div.sidebar
     (if (empty? path-list)
       [:b "jake."]
       [:a {:href "/"} "jake."])
     [:a {:href "https://isnt.online"} " ~ "]
     (collect-folder-paths path-list title)]))
