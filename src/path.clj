(ns path
  (:require
   [clojure.string :as str]
   [const :as const]))

(defn replace-extension [path extension]
  (str/replace path #"\.\S+$" extension))

(defn ->html [file]
  (replace-extension file ".html"))

(defn ->url [source-path]
  (-> source-path
      (str/replace (re-pattern const/source-dir) "")
      (str/replace (re-pattern const/target-dir) "")))

(defn source->target [path source-dir target-dir]
  (-> path
      (str/replace (re-pattern source-dir) target-dir)
      ->html))
