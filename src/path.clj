(ns path
  (:require
   const
   [clojure.string :as str]))

(defn replace-extension [path extension]
  (str/replace path #"\.\S+$" (str "." extension)))

(defn swapext [path extension]
  (replace-extension path extension))

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

;; complete a path list with a prefix
(defn complete [path-list prefix]
  (map #(str prefix "/" %) path-list))
