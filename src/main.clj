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

;; 1. Get all files in a directory of the repo
;; 2. Parse each file
;; 3. Re-path each file, get git status and metadata information, etc.
;; 3. Render each file to html

(defn main2 [opts]
  (prn "hello world"))

(def my-repo (git/load-repo "/home/jake/site"))
(git/git-status my-repo)
