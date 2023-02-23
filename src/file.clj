(ns file
  (:refer-clojure :exclude [list read])
  (:require
   [clojure.java.io :as io]))

(defn list
  "Get all the files in a directory"
  [dir]
  (let [directory (clojure.java.io/file dir)]
    ;; file-seq consistently includes the dir itself as the first argument
    (rest (file-seq directory))))

(defn read [path]
  (slurp path))

(defn write [content path]
  (spit path content :append false))
