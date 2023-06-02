(ns scss
  (:require
   const
   [command :as cmd]))

(defn ->file [from to]
  (cmd/exec (str "sass " from " " to), const/current-repo))
