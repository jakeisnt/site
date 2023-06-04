(ns filetype.scss
  (:require
   const
   [command :as cmd]))

(defn ->file [file]
  (cmd/exec (str "sass " (:source-path file) " " (:target-path file)) (:from-dir file)))
