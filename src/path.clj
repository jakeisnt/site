(ns path
  (:require
   [clojure.string :as str]
   [const :as const]))

(defn replace-extension [path extension]
  (str/replace path #"\.\S+$" extension))

(defn ->md [file]
  (replace-extension file ".md"))

(defn ->html [file]
  (replace-extension file ".html"))

(defn ->url [source-path]
  (->html (str/replace source-path (re-pattern const/source-dir) "")))

(defn path-source->target [path]
  (-> path
      (str/replace (re-pattern const/pages-source-dir) const/pages-target-dir)
      ->html))
