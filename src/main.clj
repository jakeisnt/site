(ns main
  (:require
   ;; TODO: Why doesn't 'parse' work? Why can't it be found?
   ;; [org-parser.parser :refer [parse]]
   [org-parser.core :refer [read-str write-str]]
   [hiccup.core :refer [html]]
   [clj-jgit.porcelain :as git]))

;; TODO
(defn -main [opts]
  ;; (prn (parse "* Headline"))
  (prn (read-str "* Headline"))
  (println (write-str (read-str "* Headline"))))

(defn main2 [opts]
  (prn "hello world"))

(def my-repo (git/load-repo "/home/jake/site"))
(git/git-status my-repo)
