(ns filetype.png
  (:require file))

(defn contents [file-obj]
  (file/read-image (:source-path file-obj)))

(defn ->string [file-obj]
  (contents file-obj))

(defn ->disk [file-obj]
  (file/write-image (:contents file-obj) (:target-path file-obj)))
