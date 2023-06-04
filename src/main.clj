(ns main
  (:require filetype.main file const path home index git
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

(defn file-is-new [file force-rebuild]
  (or force-rebuild
      (> (:timestamp (:last-log file)) last-commit-timestamp)))

(defn record-last-timestamp [source-dir]
  (file/write (git/last-timestamp source-dir) const/last-modified-file))

(defn compile-file
  "Compile a file to an AST, adding the contents as metadata"
  [source-dir target-dir file files file-list-idx force-rebuild]
  (let [file (if (:has-info file) file (filetype.main/info file source-dir target-dir))]
    (when (file-is-new file force-rebuild)
      (println "  Compiling: " (:source-path file))
      (filetype.main/->html file files file-list-idx))))

(defn compile-directory
  "Compile a directory to ASTs that can be written to disk"
  [source-dir target-dir files force-rebuild]
  (file/make-directory target-dir)
  (let [dir-info (filetype.main/info source-dir source-dir target-dir)]
    (if (file-is-new dir-info force-rebuild)
      (let [files
            (for [[file-list-idx file] (map-indexed vector files)]
              (compile-file source-dir target-dir file files file-list-idx force-rebuild))]
        (assoc
         dir-info
         :children files
         :contents (index/->file source-dir target-dir files)))
      dir-info)))

(defn compile-wiki-path
  "Compile all of the files at a wiki path to ASTs to write to disk"
  [config force-rebuild source-dir target-dir]
  (let [path (:folder config)
        source-path (str source-dir "/" path)
        target-path (str target-dir "/" path)
        files-to-show (if (:files-to-show config)
                        (path/complete (:files-to-show config)  source-path)
                        (file/tree source-path))
        files (map
               (fn [file] (filetype.main/info file source-dir target-dir))
               files-to-show)
        sorted-files (sort-files-by-key files (:sort-by config))]

    (println "Compiling files from '" source-path "' to '" target-path "'")
    (compile-directory source-path target-path sorted-files force-rebuild)))

(defn compile-home-page [target-dir]
  (println "Writing home page")
  (file/write (home/html) (str target-dir "/index.html")))

(defn -main [& args]
  (let [force-rebuild false ;; (some #(= % "all") args)
        target-dir "/home/jake/site/docs"]

    (doseq [source (:sources const/website)]
      (doseq [path-config (:paths source)]
        (compile-wiki-path path-config force-rebuild (:dir source) target-dir)))

    (compile-home-page target-dir)
    (record-last-timestamp target-dir)))
