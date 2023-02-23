(ns git
  (:require
   [clojure.string :as str]
   [clojure.java.shell :as sh]))

;; clojure git configuration
;; https://gist.github.com/rnesytov/b944bf9f681b0c42519cd0b6ab8f44e5
(defn run-command [command & {:keys [directory] :or {directory (System/getProperty "user.dir")}}]
  (sh/with-sh-dir directory
    (sh/sh "sh" "-c" command)))

(defn resolve-command [command & [args]]
  (reduce
   (fn [command arg-key]
     (str/replace command (str "%" (name arg-key)) (arg-key args)))
   command
   (keys args)))

(defn wrapper [command path & [args]]
  (let [result (run-command (resolve-command command args) :directory path)]
    (if (zero? (:exit result))
      (:out result)
      false)))

;; TODO: Figure out how to get git history of org-mode files as well. I don't want to lose it!
(defn lastmod [file source-dir]
  (str/split
   (wrapper (str "git log --follow --format=%ad --date default --date=format:'%Y-%m-%d' ./" file) source-dir)
   #"\R"))

;; ASSUMES file is in relative path to wiki repo
(defn log [file source-dir]
  (let [res (wrapper (str "git log --all --full-history --pretty=\"format:%h %H %ad\" --date default --date=format:'%Y-%m-%d' " source-dir "/" file) source-dir)]
    (if res
      (for [line (str/split res #"\R")]
        (let [[short-hash long-hash commit-date] (str/split line #" ")]
          {:short-hash short-hash
           :long-hash long-hash
           :commit-date commit-date
           :file-path file}))
      (throw (Throwable. (str "git log command failed on path " file))))))
