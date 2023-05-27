(ns main
  (:require file markdown const path act home index git
            [clojure.core.match :refer [match]]
            [clojure.string :refer [trim-newline]]))

;; the commit on which the file was last built
(def last-commit-timestamp
  (if (file/exists? const/last-modified-file)
    (Integer/parseInt (trim-newline (file/read const/last-modified-file)))
    ;; if file doesn't exist, we assume it's 0 and recompile
    0))

(println "Last build was at " last-commit-timestamp)

(defn info [file]
  (let [last-log (git/last-log (file/path file) const/source-dir)]
    {:file file :last-log last-log :name (file/name file)}))

;; TODO: goes the wrong way
;; TODO: customize behavior by folder
(defn sort-files-by-key [files key]
  (let [file-list (for [file files] (info file))]
    (reverse (sort-by key file-list))))

(defn file-is-new [source-dir source-path force-rebuild]
  (or force-rebuild
      (> (git/last-timestamp source-dir source-path) last-commit-timestamp)))

(defn record-last-timestamp [source-dir]
  (file/write (git/last-timestamp source-dir) const/last-modified-file))

(defn make-dir-files [source-dir target-dir files force-rebuild]
  (doseq [file files]
    (let [source-path (:file file)]
      (when (file-is-new source-dir source-path force-rebuild)
        (println "Rebuilding updated file " (str source-path))
        (let [target-path (path/->html (path/source->target source-path source-dir target-dir))]
          (match (file/extension source-path)
            "md"  (markdown/->file source-path target-path)
            "act" (act/->file source-path target-path)))))))

(defn make-dir
  "Make a directory listing page"
  [source-dir target-dir files force-rebuild]
  (file/make-directory target-dir)
  (when (file-is-new source-dir source-dir force-rebuild)
    (make-dir-files source-dir target-dir files force-rebuild)
    (index/->file source-dir target-dir files)))

(defn write-home []
  (println "Writing home page")
  (file/write (home/html) (str const/target-dir "/index.html")))

(defn write-path [config force-rebuild]
  (let [path (:folder config)
        source-path (str const/source-dir "/" path)
        target-path (str const/target-dir "/" path)
        sort-by (:sort-by config)
        files (file/list source-path)
        sorted-files (sort-files-by-key files sort-by)]
    (println "Writing path:" path)
    (make-dir
     source-path
     target-path
     sorted-files
     force-rebuild)))

(defn copy-resources []
  (println "Copying resources")
  (file/copy-force (str const/current-repo "/resources/*") const/target-dir const/current-repo)
  (file/copy-force (str const/current-repo "/components") const/target-dir const/current-repo))

(defn -main [& args]
  (let [force-rebuild true ;; (some #(= % "all") args)
        ]
    (copy-resources)
    (write-home)
    (doseq [path const/wiki-paths]
      (write-path path force-rebuild))
    (record-last-timestamp const/source-dir)))
