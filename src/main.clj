(ns main
  (:require file markdown const path script home index
            [clojure.core.match :refer [match]]))

(defn make-dir-files [source-dir target-dir]
  (let [files (file/list source-dir)]
    (doseq [file files]
      (match (file/extension file)
        "md"  (markdown/->file file source-dir target-dir)
        "act" (script/->file file source-dir target-dir)))))

(defn make-dir
  "Make a directory listing page"
  [source-dir target-dir]
  (make-dir-files source-dir target-dir)
  (index/->file source-dir target-dir))

(defn -main [_]
  (file/write (home/html) (str const/target-dir "/index.html"))

  (for [path const/wiki-paths]
    (make-dir (str const/source-dir "/" path) (str const/target-dir "/" path))))

(comment (make-dir (str const/source-dir "/" "scripts") (str const/target-dir "/" "scripts")))
(comment (-main nil))
