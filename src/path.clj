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

(defn source->target [path source-dir target-dir]
  (-> path
      (str/replace (re-pattern source-dir) target-dir)
      ->html))
