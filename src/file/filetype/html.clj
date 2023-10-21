(ns filetype.html
  (:require file html))

(defn ->string [file-obj]
  (html/->string (:contents file-obj)))

(defn ->disk [file-obj]
  (file/write (->string file-obj) (:target-path file-obj)))
