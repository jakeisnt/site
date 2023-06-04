(ns deploy
  (:require file const path home index git
            [main]))

(defn commit-folder-to [v]
  (let [repo (:repo v)
        deployment-dir (:deployment-dir v)
        branch (:branch v)
        current-branch (git/current-branch repo)
        tmp-dir "/tmp/jake-site-deploy"]

    (println "saving current changes")
    (git/add-all repo)
    (git/stash repo)

    (println "copying deployment to tmp dir")
    (git/checkout repo branch)
    (file/move deployment-dir tmp-dir repo)
    (git/status repo)

    (println "removing all untracked files")
    (git/remove-untracked repo)
    (git/status repo)

    (println "moving tmp dir contents to root")

    (file/copy-dir tmp-dir deployment-dir repo)
    (file/copy-dir (str deployment-dir "/*") "." repo)
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
    (file/move tmp-dir deployment-dir repo)

    (git/stash-pop repo)))

(defn -main [_]
  (let [current-repo "/home/jake/site"
        deployment-branch const/deployment-branch
        target-dir const/target-dir]
    (main/-main nil)
    (commit-folder-to {:repo current-repo
                       :branch deployment-branch
                       :deployment-dir target-dir})))
