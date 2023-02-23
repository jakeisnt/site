(ns main
  (:require
   [org-parser.core :refer [read-str]]
   ;; [clj-org.org :refer [parse-org]]
   ;; [hiccup.core :refer [html]]
   ;; [clojure.core.match :refer-macros [match]]
   ;; [clojure.string :as str]
   ;; [clj-jgit.porcelain :as git]
   )
  (:import
   (com.orgzly.org.parser OrgParser.Builder)))

;; TODO: Why doesn't 'parse' work? Why can't it be found?
;; [org-parser.parser :refer [parse]]

;; TODO
;; (defn -main [opts]
;;   ;; (prn (parse "* Headline"))
;;   (prn (read-str "* Headline"))
;;   (println (write-str (read-str "* Headline"))))

(defn read-file [path]
  (slurp path))

(defn parse-org [org]
  (->
   (new OrgParser.Builder)
   (.setTodoKeywords ["TODO" "DONE"])
   (.build)
   (.parse org)))

(->
 (read-file "/home/jake/site/src/test.org")
 (parse-org))
