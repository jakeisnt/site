(ns filetype.css
  (:require filetype.scss file))

(defn ->string [file-struct]
  (filetype.scss/->disk file-struct)
  (file/read (:target-path file-struct)))

(defn ->disk [file-struct]
  (filetype.scss/->disk file-struct))
