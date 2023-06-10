(ns main
  (:require filetype.main file const path home git
            [clojure.string :refer [trim-newline]]))

;; the commit on which the file was last built
(def last-commit-timestamp
  (if (file/exists? const/last-modified-file)
    (Integer/parseInt (trim-newline (file/read const/last-modified-file)))
    ;; if file doesn't exist, we assume it's 0 and recompile
    0))

(println "Last build was at " last-commit-timestamp)

;; assumes we have file info
(defn sort-files-by-key [files key]
  (if (not key)
    files
    (reverse (sort-by key files))))

(defn file-is-new [file config]
  (or (:force-rebuild config)
      (> (:timestamp (:last-log file)) last-commit-timestamp)))

(defn record-last-timestamp [source-dir]
  (file/write (git/last-timestamp source-dir) const/last-modified-file))

(defn get-dir-files [source-dir target-dir config]
  (let [files-to-show (file/list-dir source-dir)
        files (map
               (fn [file-x] (filetype.main/info file-x source-dir target-dir))
               files-to-show)
        sorted-files (sort-files-by-key files (:sort-by config))]
    sorted-files))

(defn compile-file
  "Compile a file to an AST, adding the contents as metadata"
  [file-obj files file-list-idx _]
  (println "Compiling file " (:source-path file-obj))
  (filetype.main/with-contents file-obj files file-list-idx))

(declare compile-unit)

(defn compile-directory
  "Compile a directory to ASTs that can be written to disk"
  [dir-info adjacent-files adjacent-idx config]
  (println "Compiling directory: " (:source-path dir-info))
  (let [pre-files (get-dir-files (:source-path dir-info) (:target-path dir-info) config)
        compiled-files (for [[file-list-idx file] (map-indexed vector pre-files)]
                         (compile-unit file pre-files file-list-idx config))]
    (filetype.main/with-contents
      (assoc dir-info :children compiled-files) adjacent-files adjacent-idx)))

(defn compile-unit [file-info-obj adjacent-files adjacent-idx config]
  (if (file-is-new file-info-obj config)
    (do
      (println "  Compiling: " (:source-path file-info-obj))
      ((if (:is-directory file-info-obj) compile-directory compile-file)
       file-info-obj adjacent-files adjacent-idx config))
    (do
      (println "  Skipping: " (:source-path file-info-obj))
      file-info-obj)))

(defn compile-wiki-path
  "Compile all of the files at a wiki path to ASTs to write to disk"
  [config source-dir target-dir]
  (let [path (:folder config)
        source-path (str source-dir "/" path)
        target-path (str target-dir "/" path)
        dir-info (filetype.main/info source-path source-dir target-dir)]

    (println "Compiling files from '" source-path "' to '" target-path "'")
    (compile-unit dir-info '() nil config)))

(defn compile-home-page [target-dir]
  (println "Writing home page")
  (home/->disk target-dir))

(defn -main [& args]
  (let [target-dir "/home/jake/site/docs"
        compiled-site
        (doall (for [source (:sources const/website)]
                 (doall (for [path-config (:paths source)]
                          (filetype.main/->disk
                           (compile-wiki-path path-config (:dir source) target-dir))))))]

    (compile-home-page target-dir)

    (record-last-timestamp target-dir)))
