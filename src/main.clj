(ns main
  (:require file markdown const path act home index
            [clojure.core.match :refer [match]]))

(defn make-dir-files [source-dir target-dir]
  (let [files (file/list source-dir)]
    (doseq [source-path files]
      (let [target-path (path/source->target source-path source-dir target-dir)]
        (match (file/extension source-path)
          "md"  (markdown/->file source-path target-path)
          "act" (act/->file source-path target-path))))))

(defn make-dir
  "Make a directory listing page"
  [source-dir target-dir]
  (make-dir-files source-dir target-dir)
  (index/->file source-dir target-dir))

(defn write-home []
  (println "Writing home page")
  (file/write (home/html) (str const/target-dir "/index.html")))

(defn write-path [path]
  (println "Writing path:" path)
  (make-dir (str const/source-dir "/" path) (str const/target-dir "/" path)))

(defn -main [_]
  (write-home)
  (doseq [path const/wiki-paths]
    (write-path path)))

(comment (write-home))
(comment (make-dir (str const/source-dir "/" "scripts") (str const/target-dir "/" "scripts")))
(comment (-main nil))
