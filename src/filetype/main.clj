(ns filetype.main
  (:require
   filetype.scss filetype.markdown filetype.act filetype.html filetype.css
   file path git
   [clojure.core.match :refer [match]]))

(defn target-extension [source-path]
  (let [extension (file/extension source-path)]
    (match extension
      "scss" "css"
      "md"   "html"
      "act"  "html"
      :else extension)))

(defn info
  "Provided a source file path, its dir, and a target dir,
    assemble a map of information about the file."
  [file-obj source-dir target-dir]
  (let [file-path (file/path file-obj)
        last-log (git/last-log file-path source-dir)
        target-extension (filetype.main/target-extension file-path)
        target-path (path/swapext (path/source->target file-path source-dir target-dir) target-extension)]
    {:file file-obj
     :has-info true
     :from-dir source-dir
     :source-path file-path
     :target-path target-path
     :source-extension (file/extension file-path)
     :target-extension target-extension
     :link (path/remove-prefix target-path target-dir)
     :last-log last-log
     :name (file/name file-obj)}))

(defn ->html
  "Parse a file's contents to an AST, adding :contents to the file struct"
  [file-struct files file-list-idx]
  (assoc file-struct
         :contents
         (match (:source-extension file-struct)
           "scss" (filetype.scss/->file file-struct)
           "md"   (filetype.markdown/->file file-struct files file-list-idx)
           "act"  (filetype.act/->file file-struct files file-list-idx)
           "png" (file/read-image (:source-path file-struct))
           :else  (file/copy (:source-path file-struct)
                             (:target-path file-struct)
                             (:from-dir file-struct)))))

(defn ->string
  "Serialize a file struct to a string"
  [file-struct]
  (match (:target-extension file-struct)
    "html" (filetype.html/->string file-struct)
    "css"  (filetype.css/->string file-struct)
    "png"  (file/read-image (:source-path file-struct))
    :else  (file/read (:source-path file-struct))))

(defn ->disk
  "Write a file to its known target path."
  [file-struct]
  (file/write (:target-path file-struct) (:contents file-struct)))
