(ns main
  (:require file markdown const path script home index
            [clojure.core.match :refer [match]]))

;; TODO
;; to get up to par with the common lisp build:
;; - implement calendar on home page

;; To improve:
;; - Journals page

(defn make-dir-files [source-dir target-dir]
  (let [files (file/list source-dir)]
    (doseq [file files]
      (match (file/extension file)
        "md"  (markdown/->file file source-dir target-dir)
        "act" (script/->file file source-dir target-dir)))))

(defn make-dir
  "Make a directory listing page"
  [source-dir target-dir]
  (let [target-path (str (path/source->target source-dir source-dir target-dir) "/index.html")]
    (make-dir-files source-dir target-dir)
    (index/->file source-dir target-path)))

(defn -main [_]
  (file/write (home/html) (str const/target-dir "/index.html"))
  (make-dir const/pages-source-dir const/pages-target-dir)
  (make-dir const/script-source-dir const/script-target-dir))

(-main nil)
