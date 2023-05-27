(ns html
  (:require
   path const
   [hiccup.core :as h]
   [clojure.core.match :refer [match]]))

;; --- general ---

;; delimits paths in title and in body
(def ^:const path-delimiter " / ")

(defn metm [k v]
  [:meta {:name k :content v}])

(defn prop [k v]
  [:meta {:property k :content v}])

(defn defer-script
  ([src] [:script {:src src :id src :defer true}]))

(defn script
  ([src] [:script {:src src :id src}])
  ([src body] [:script {:src src :id src} body]))

(defn css
  ([href] [:link {:rel "stylesheet" :type "text/css" :href href :id href}])
  ([href body] [:link {:rel "stylesheet" :type "text/css" :href href :id href} body]))

(defmacro favicon []
  [:link {:rel "icon" :type "image/x-icon" :href "/favicon/favicon.ico"}]
  [:link {:rel "apple-touch-icon" :href "/favicon/apple-touch-icon.png"}])

(defn make-path-list [path]
  (path/split (path/remove-prefix path const/target-dir)))

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
   [:meta {:name "theme-color" :media "(prefers-color-scheme: light)" :content "white"}]
   [:meta {:name "theme-color" :media "(prefers-color-scheme: dark)" :content "#111"}]
   (favicon)

   [:link {:rel "manifest" :href "/manifest.json"}]

   (css "/style.css")
   (css "/global.css")
   (script "/lib.js")

   (css "/elements.css")
   (defer-script "/elements.js")

   ;; TODO: move to code block enhancements
   (defer-script "https://unpkg.com/@highlightjs/cdn-assets@11.7.0/highlight.min.js")
   [:link {:rel "stylesheet" :id "dark-theme-highlight" :href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/styles/nord.min.css"}]
   [:link {:rel "stylesheet" :id "light-theme-highlight" :href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/styles/atom-one-light.min.css"}]])

(defn ->string
  "Serialize Hiccup-compatible data to a string"
  [hiccup-struct]
  (str "<!DOCTYPE html>" (h/html hiccup-struct)))

;; find all of the elements that fit a predicate
(defn collect-elements [html-page pred]
  (filter pred (drop 2 html-page)))

;; collect all of the html tags with a given id
(defn find-tags [html-page & tags]
  (collect-elements html-page (fn [html-block]
                                (let [tag (first html-block)]
                                  (some #(= tag %) tags)))))

(defn heading-rank [heading-tag]
  (match heading-tag
    "h1" 1
    "h2" 2
    "h3" 3
    "h4" 4
    "h5" 5
    "h6" 6
    :else 7))

(defn heading-hierarchy
  ([tag-list] (heading-hierarchy tag-list 0))
  ([tag-list rank]
   (cond
     (empty? tag-list) '()
     :else
     (let [first-heading (first tag-list)
           [cur-headings next-headings]
           (split-with #(= (heading-rank %) rank) (rest tag-list))]
       (concat
        ;; if the first heading is the level we're looking for,
        ;; we construct it
        (if (= (heading-rank (first first-heading)) rank)
          (list {:tag (first first-heading)
                 :id (:id (second first-heading))
                 :depth heading-rank
                 :children (heading-hierarchy cur-headings (inc rank))})
          ;; if the first heading is not the level we're looking for,
          ;; we increment the rank and try again
          (heading-hierarchy (cons first-heading cur-headings) (inc rank)))

        ;; join this to the heading hierarchy of the rest of the headings
        (heading-hierarchy next-headings rank))))))

(defn heading-hierarchy->html [hierarchy]
  (map (fn [heading]
         (let [children (:children heading)]
           (if (empty? children)
             [:div.hierarchy-part
              {:class (str "hierarchy-part " (:depth heading))}
              [:span.hierarchy-pipe "├╴"] [:a {:id (:id heading)} [:span.hierarchy-text (:id heading)]]]

             [:a {:id (:id heading)} (heading-hierarchy->html children)])))
       hierarchy))
