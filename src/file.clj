(ns file
  (:refer-clojure :exclude [list read name])
  (:require
   const
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [command :as cmd]))

(defn list
  "Get all the files in a directory"
  [dir]
  (let [directory (clojure.java.io/file dir)]
    ;; file-seq consistently includes the dir itself as the first argument
    (rest (file-seq directory))))

(defn read [path]
  (slurp path))

(defn read-image [path]
  (io/input-stream path))

(defn make-directory [dir]
  (.mkdir (clojure.java.io/file dir)))

(defn write [content path]
  (spit path content :append false))

(defn path
  "get the path to a file"
  [file]
  (if (= (type file) java.lang.String)
    file
    (str (.getPath file))))

(defn title
  "Retrieve the file name, with extension included, from a path"
  [p]
  (-> p
      path
      (str/split #"/")
      reverse
      first))

(defn name
  "Get the file name, without the extension, from a path"
  [p]
  (-> p
      title
      (str/split #"[.]")
      first))

(defn extension
  "Get the file extension from a path"
  [p]
  (-> p
      title
      (str/split #"[.]")
      second))

(defn dir?
  "Is this path a directory?"
  [path]
  (.isDirectory (java.io.File. path)))

(defn directory? [path]
  (dir? path))

(defn move
  ([from to] (move from to const/current-repo))
  ([from to dir] (cmd/exec (str "mv " from " " to) dir)))

(defn copy-dir [from to dir]
  (cmd/exec (str "cp -r " from " " to) dir))

(defn compile-scss [from to]
  (cmd/exec (str "sass " from " " to), const/current-repo))

(defn copy [from to]
  (copy-dir from to const/current-repo))

(defn copy-force
  ([from to] (copy-force from to const/current-repo))
  ([from to dir] (cmd/exec (str "cp -rf " from " " to) dir)))

(defn remove-dir
  ([path] (remove-dir path const/current-repo))
  ([path in-dir] (cmd/exec (str "rm -r " path) in-dir)))

(defn exists? [fp]
  (.exists (io/file fp)))

(defn load-edn
  "Load edn from an io/reader source (filename or io/resource)."
  [source]
  (try
    (with-open [r (io/reader source)]
      (edn/read (java.io.PushbackReader. r)))

    (catch java.io.IOException e
      (printf "Couldn't open '%s': %s\n" source (.getMessage e)))
    (catch RuntimeException e
      (printf "Error parsing edn file '%s': %s\n" source (.getMessage e)))))
