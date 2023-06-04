(ns filetype.html
  (:require html))

(defn ->string [file-obj]
  (html/->string (:contents file-obj)))
