(ns html
  (:require
   path const
   [hiccup.core :as h]
   [clojure.core.match :refer [match]]))

;; --- general ---

;; delimits paths in title and in body
(def ^:const path-delimiter " / ")

(defn make-path-list [path]
  (path/split (path/remove-prefix path const/target-dir)))

(defn collect-folder-paths-string [path-list title]
  (if (empty? (rest path-list))
    title
    (str (collect-folder-paths-string (rest path-list) title) path-delimiter (first path-list))))

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
