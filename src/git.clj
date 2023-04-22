(ns git
  (:require
   [clojure.string :as str]
   [const :as const]
   [file :as file]
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
;; ASSUMES file is in relative path to wiki repo
(defn log [file source-dir]
  (let [file (str file)]
    (let [res (wrapper (str "git log --all --full-history --pretty=\"format:%h %H %ad\" --date default --date=format:'%Y-%m-%d' " (str file)) source-dir)]
      (if res
        (for [line (str/split res #"\R")]
          (let [[short-hash long-hash commit-date] (str/split line #" ")]
            {:short-hash short-hash
             :long-hash long-hash
             :commit-date commit-date
             :file-path file}))
        (throw (Throwable. (str "git log command failed on path " file)))))))

;; get the commit during which a file (or repo, if no file provided) was last modified
(defn last-commit-hash
  ([source-dir] (wrapper (str  "git log -1  --pretty=format:%H") source-dir))
  ([source-dir path] (wrapper (str  "git log -1  --pretty=format:%H --follow -- " (str path)) source-dir)))

(defn last-timestamp
  ([source-dir] (Integer/parseInt (wrapper (str  "git log -1  --pretty=format:%ct") source-dir)))
  ([source-dir path] (Integer/parseInt (wrapper (str  "git log -1  --pretty=format:%ct --follow -- " (str path)) source-dir))))

(defn history-link
  "Generate a link to git history"
  [long-hash file-path]
  (str const/source-url "/blob/" long-hash "/" file-path))

(defn history-table
  "Renders the git history for a file given its path."
  [source-path]
  (let [git-log (git/log (file/path source-path) const/source-dir)]
    [:div.git-hist-table
     [:table
      [:tbody
       (for [commit git-log]
         [:tr
          [:td (:commit-date commit)]
          [:td [:a {:href (history-link (:long-hash commit) (:file-path commit))} (:short-hash commit)]]])]]]))
