(ns main
  (:require scss file markdown const path act home index git
            [clojure.core.match :refer [match]]
            [clojure.string :refer [trim-newline]]))

;; the commit on which the file was last built
(def last-commit-timestamp
  (if (file/exists? const/last-modified-file)
    (Integer/parseInt (trim-newline (file/read const/last-modified-file)))
    ;; if file doesn't exist, we assume it's 0 and recompile
    0))

(println "Last build was at " last-commit-timestamp)

(defn info [file source-dir]
  (let [file-path (file/path file)
        last-log (git/last-log file-path source-dir)]
    {:file file
     :from-dir source-dir
     :source-path file-path
     :target-path (path/source->target file-path source-dir const/target-dir)
     :link (path/swapext (path/remove-prefix file source-dir) "html")
     :last-log last-log :name (file/name file)}))

;; get info for files
(defn fill-info [files source-dir]
  (map (fn [file] (info file source-dir)) files))

;; assumes we have file info
(defn sort-files-by-key [files key]
  (reverse (sort-by key files)))

(defn file-is-new [source-dir source-path force-rebuild]
  (or force-rebuild
      (> (git/last-timestamp source-dir source-path) last-commit-timestamp)))

(defn record-last-timestamp [source-dir]
  (file/write (git/last-timestamp source-dir) const/last-modified-file))

(defn compile-file
  "Compile a file, returning its file config with file metadata added"
  [source-dir target-dir file files file-list-idx force-rebuild]

  (let [source-path (:source-path file)]
    (when (file-is-new source-dir source-path force-rebuild)
      (println "Rebuilding updated file " (str source-path))
      (let [target-path (path/source->target source-path source-dir target-dir)]
        (assoc file
               :contents
               (match (file/extension source-path)
                 "scss" (scss/->file file source-path (path/swapext target-path "css"))
                 "md"   (markdown/->file
                         source-path
                         (path/swapext target-path "html")
                         file
                         files
                         file-list-idx)
                 "act"  (act/->file source-path (path/swapext target-path "html"))
                 :else   (file/copy source-path target-path (:from-dir file))))))))

(defn compile-directory-v2
  "Make a directory listing page"
  [source-dir target-dir files force-rebuild]
  (file/make-directory target-dir)
  (when (file-is-new source-dir source-dir force-rebuild)
    (let [files
          (for [[file-list-idx file] (map-indexed vector files)]
            (compile-file source-dir target-dir file files file-list-idx force-rebuild))]
      ;; TODO: should return a 'directory' object with the html of the index page and the directory
      ;; because we iterate through a tree of files, this isn't a simple map.
      (index/->file source-dir target-dir files)
      files)))

(defn compile-home-page []
  (println "Writing home page")
  (file/write (home/html) (str const/target-dir "/index.html")))

(defn compile-wiki-path [config force-rebuild source-dir]
  (let [path (:folder config)
        source-path (str source-dir "/" path)
        target-path (str const/target-dir "/" path)
        sort-by (:sort-by config)
        files-to-show (:show-only config)
        files (fill-info (if files-to-show
                           (path/complete files-to-show source-path)
                           (file/tree source-path))
                         source-dir)
        sorted-files (if sort-by
                       (sort-files-by-key files sort-by)
                       files)]
    (println "Writing path:" path)
    (compile-directory-v2
     source-path
     target-path
     sorted-files
     force-rebuild)))

;; TODO: should these `for` forms write files to disk at all?
;; maybe that should be done at the end?
(defn -main [& args]
  (let [force-rebuild true ;; (some #(= % "all") args)
        ]

    (doseq [source (:sources const/website)]
      (doall (for [path (:paths source)]
               (compile-wiki-path path force-rebuild (:dir source)))))

    (compile-home-page)
    (record-last-timestamp "/home/jake/wiki")))
