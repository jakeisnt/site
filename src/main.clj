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
    {:file file
     :source-path (file/path file)
     :target-path (path/source->target (file/path file) const/source-dir const/target-dir)
     :link (path/->html (path/->url file))
     :last-log last-log :name (file/name file)}))

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

(defn make-dir-file [source-dir target-dir file files file-list-idx force-rebuild]
  (let [source-path (:file file)]
    (when (file-is-new source-dir source-path force-rebuild)
      (println "Rebuilding updated file " (str source-path))
      (let [target-path (path/source->target source-path source-dir target-dir)]
        (match (file/extension source-path)
          "scss" (file/compile-scss source-path (path/swapext target-path "css"))
          "md"   (markdown/->file
                  source-path
                  (path/swapext target-path "html")
                  file
                  files
                  file-list-idx)
          "act"  (act/->file source-path (path/swapext target-path "html"))
          true   (file/copy source-path target-path))))))

;; move the file from the 'to' path to the 'from' path,
;; applying any transformations we might want
;; note that the 'to' path file extension can be changed by this function
;; (it doesn't really matter?)
(defn compile-file [from-file to-file]
  (let [extension (file/extension from-file)]
    (if (= extension "scss")
      (file/compile-scss from-file (path/replace-extension to-file "css"))
      (file/copy from-file to-file))))

;; compile all of the files in a given directory recursively
(defn compile-directory [from-dir to-dir]
  (println "=== Compiling directory ===" from-dir)
  (doseq [file (file/tree from-dir)]
    (println "Compiling file " (file/path file))
    (let [from-file (file/path file)
          to-file (path/source->target from-file from-dir to-dir)]
      (if (file/directory? from-file)
        (println "File is a directory. Ignoring " from-file)
        (compile-file from-file to-file)))))

(defn compile-directory-v2
  "Make a directory listing page"
  [source-dir target-dir files force-rebuild]
  (file/make-directory target-dir)
  (when (file-is-new source-dir source-dir force-rebuild)
    (doseq [[file-list-idx file] (map-indexed vector files)]
      (make-dir-file source-dir target-dir file files file-list-idx force-rebuild))

    (index/->file source-dir target-dir files)))

(defn compile-home-page []
  (println "Writing home page")
  (file/write (home/html) (str const/target-dir "/index.html")))

(defn compile-wiki-path [config force-rebuild]
  (let [path (:folder config)
        source-path (str const/source-dir "/" path)
        target-path (str const/target-dir "/" path)
        sort-by (:sort-by config)
        files-to-show (:show-only config)
        files (if files-to-show
                (path/complete files-to-show source-path)
                (file/tree source-path))
        sorted-files (sort-files-by-key files sort-by)]
    (println "Writing path:" path)
    (compile-directory-v2
     source-path
     target-path
     sorted-files
     force-rebuild)))

(defn copy-resources []
  (println "Copying resources")
  (compile-directory (str const/current-repo "/resources") const/target-dir)
  (compile-directory (str const/current-repo "/components") const/target-dir))

(defn -main [& args]
  (let [force-rebuild true ;; (some #(= % "all") args)
        ]
    (copy-resources)
    (for [path const/wiki-paths]
      (compile-wiki-path path force-rebuild))
    (compile-home-page)
    (record-last-timestamp const/source-dir)))
