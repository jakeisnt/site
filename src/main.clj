(ns main
  (:require file markdown const path act home index git
            [clojure.core.match :refer [match]]
            [clojure.string :refer [trim-newline]]))

;; manually override timestamp cache if set to false
(def respect-timestamp true)
;; the commit on which the file was last built
(def last-commit-timestamp
  (Integer/parseInt (trim-newline (file/read const/last-modified-file))))

(println "Last build was at timestamp: " last-commit-timestamp)

(defn file-is-new [source-dir source-path]
  (and respect-timestamp (> (git/last-timestamp source-dir source-path) last-commit-timestamp)))

(defn record-last-timestamp [source-dir]
  (file/write (git/last-timestamp source-dir) const/last-modified-file))

(defn make-dir-files [source-dir target-dir]
  (let [files (file/list source-dir)]
    (doseq [source-path files]
      (when (file-is-new source-dir source-path)
        (println "file was updated. rebuilding" (str source-path))
        (let [target-path (path/source->target source-path source-dir target-dir)]
          (match (file/extension source-path)
            "md"  (markdown/->file source-path target-path)
            "act" (act/->file source-path target-path)))))))
;;

(defn make-dir
  "Make a directory listing page"
  [source-dir target-dir]
  (file/make-directory target-dir)
  (when (file-is-new source-dir source-dir)
    (make-dir-files source-dir target-dir)
    (index/->file source-dir target-dir)))

(defn write-home []
  (println "Writing home page")
  (file/write (home/html) (str const/target-dir "/index.html")))

(defn write-path [path]
  (println "Writing path:" path)
  (make-dir (str const/source-dir "/" path) (str const/target-dir "/" path)))

(defn -main [_]
  (file/copy-force (str const/current-repo "/resources/*") const/target-dir const/current-repo)
  (write-home)
  (doseq [path const/wiki-paths]
    (write-path path))
  (record-last-timestamp const/source-dir))

(defn commit-folder-to [v]
  (let [repo (:repo v)
        deployment-dir (:deployment-dir v)
        branch (:branch v)
        current-branch (git/current-branch repo)
        tmp-dir "/tmp/jake-site-deploy"]

    (println "copying deployment to tmp dir")
    (git/checkout repo branch)
    (file/move deployment-dir tmp-dir repo)
    (git/status repo)

    (println "removing all untracked files")
    (git/remove-untracked repo)
    (git/status repo)

    (println "moving tmp dir contents to root")
    (file/copy-dir tmp-dir deployment-dir repo)
    (file/move (str deployment-dir "/*") "." repo)
    (git/status repo)

    (println "pushing build")
    (println "we are on branch " (git/current-branch repo))
    (git/add-all repo)
    (git/commit repo)
    (git/push repo)
    (git/status repo)

    (println "restoring working branch")
    (git/checkout repo current-branch)
    (git/status repo)

    (print "removing untracked")
    (git/remove-untracked repo)
    (git/status repo)

    (print "removing deployment dir")
    (file/remove-dir deployment-dir repo)
    (print "moving tmp dir to deployment dir")
    (file/move tmp-dir deployment-dir repo)))

(defn -deploy [_]
  (-main nil)
  (file/copy-force (str const/current-repo "/resources/*") const/target-dir const/current-repo)
  (commit-folder-to {:repo const/current-repo
                     :branch const/deployment-branch
                     :deployment-dir const/target-dir}))

(comment (write-home))
(comment (make-dir (str const/source-dir "/" "scripts") (str const/target-dir "/" "scripts")))
(comment (-main nil))
