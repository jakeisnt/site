(ns file
  (:refer-clojure :exclude [list read name])
  (:require
   [clojure.string :as str]
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

(defn path
  "get the path to a file"
  [file]
  (if (= (type file) java.lang.String)
    file
    (str (.getPath file))))

(defn name
  "Get the file name, without the extension, from a path"
  [p]
  (nth (reverse (str/split (path p) #"/|[.]")) 1))

(defn extension
  "Get the file extension from a path"
  [p]
  (nth (reverse (str/split (path p) #"/|[.]")) 0))

(defn dir?
  "Is this path a directory?"
  [path]
  (.isDirectory path))
