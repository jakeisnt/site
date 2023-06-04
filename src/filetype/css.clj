(ns filetype.css
  (:require filetype.scss file))

(defn ->string [file-struct]
  (filetype.scss/->file file-struct)
  (file/read (:target-path file-struct)))
