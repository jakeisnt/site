(ns filetype.scss
  (:require
   const
   [command :as cmd]))

;; read the scss file in as string
;; later we will want this to be the sass sourcemap as edn
;; so we can statically snag classnames from modules
(defn contents [file-struct]
  (slurp (:source-path file-struct)))

(defn ->disk [file]
  (cmd/exec (str "sass " (:source-path file) " " (:target-path file)) (:from-dir file)))

(defn ->string [file]
  (->disk file)
  (slurp (:target-path file)))
