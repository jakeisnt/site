(ns path
  (:require
   const
   [clojure.string :as str]))

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
      (str/replace (re-pattern source-dir) target-dir)))

(defn folder [path]
  (let [parts (str/split path #"/")]
    (str/join "/" (take (dec (count parts)) parts))))

(defn remove-prefix [path prefix]
  (-> path
      (str/replace (re-pattern (str "^" prefix)) "")))

(defn split [path]
  ;; splitting on the first /" gives us a front empty string that we drop
  (rest (str/split path #"/")))
