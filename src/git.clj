(ns git
  (:require
   const file path
   [clojure.string :as str]
   [command :as cmd]))

;; TODO: Figure out how to get git history of org-mode files as well. I don't want to lose it!
;; ASSUMES file is in relative path to wiki repo
(defn log [file source-dir]
  (let [file (str file)
        res (cmd/exec (str "git log --all --full-history --pretty=\"format:%h %H %ad\" --date default --date=format:'%Y-%m-%d' " (str file)) source-dir)]
    (if res
      (for [line (str/split res #"\R")]
        (let [[short-hash long-hash commit-date] (str/split line #" ")]
          {:short-hash short-hash
           :long-hash long-hash
           :commit-date commit-date
           :file-path file}))
      (throw (Throwable. (str "git log command failed on path " file))))))

;; ASSUMES file is in relative path to wiki repo
(defn last-log [file source-dir]
  (let [file (str file)
        res (cmd/exec (str "git log -1 --full-history --pretty=\"format:%h %H %ad %ct\" --date default --date=format:'%Y-%m-%d' " (str file)) source-dir)]
    (if res
      (let [line (first (str/split res #"\R"))
            [short-hash long-hash commit-date timestamp] (str/split line #" ")]
        {:short-hash short-hash
         :long-hash long-hash
         :commit-date commit-date
         :timestamp (Integer/parseInt timestamp)
         :file-path file})
      (throw (Throwable. (str "git log command failed on path " file))))))

(defn last-timestamp
  ([source-dir] (Integer/parseInt (cmd/exec (str  "git log -1  --pretty=format:%ct") source-dir)))
  ([source-dir path] (Integer/parseInt (cmd/exec (str  "git log -1  --pretty=format:%ct --follow -- " (str path)) source-dir))))

(defn history-link
  "Generate a link to git history"
  [long-hash file-path]
  (str const/source-url "/blob/" long-hash "/" file-path))

(defn checkout [source-dir branch]
  (cmd/exec (str "git checkout " branch) source-dir))

(defn add-all [source-dir]
  (cmd/exec (str "git add .") source-dir))

(defn commit [source-dir]
  (cmd/exec  "git -c commit.gpgsign=false commit -m \"robot commit\"" source-dir))

(defn push [source-dir]
  (cmd/exec  "git push" source-dir))

(defn remove-untracked [dir]
  (cmd/exec "git clean -fxd" dir))

(defn current-branch [dir]
  (cmd/exec "git branch --show-current" dir))

(defn status [dir]
  (println (cmd/exec "git status" dir)))

(defn stash [dir]
  (cmd/exec "git stash" dir))

(defn stash-pop [dir]
  (cmd/exec "git stash pop" dir))
