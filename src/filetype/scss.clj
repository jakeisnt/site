(ns filetype.scss
  (:require
   const
   [command :as cmd]))

(defn ->file [file from to]
  (cmd/exec (str "sass " from " " to), (:from-dir file)))
