(ns filetype.main
  (:require
   filetype.directory filetype.scss filetype.markdown filetype.act filetype.html filetype.css filetype.png
   file path git
   [clojure.core.match :refer [match]]))

(defn target-extension [src-extension]
  (match src-extension
    "scss" "css"
    "md"   "html"
    "act"  "html"
    :else src-extension))

(defn info
  "Provided a source file path, its dir, and a target dir,
    assemble a map of information about the file."
  [file-obj source-dir target-dir]
  (let [file-path (file/path file-obj)
        last-log (git/last-log file-path source-dir)
        src-extension (file/extension file-path)
        target-extension (filetype.main/target-extension src-extension)
        target-path (path/swapext
                     (path/source->target file-path source-dir target-dir)
                     target-extension)]

    {:file file-obj
     :has-info true
     :from-dir source-dir
     :source-path file-path
     :target-path target-path
     :source-extension (or src-extension "directory")
     :target-extension target-extension
     :link (path/remove-prefix target-path target-dir)
     :last-log last-log
     :name (file/name file-obj)}))

(defn with-contents
  "Parse a file's contents to an AST, adding :contents to the file struct.
   Source files export a function, 'contents', that produces the file's contents
   as an AST of some target type.
  "
  [file-struct files file-list-idx]
  (assoc file-struct
         :contents
         (match (:source-extension file-struct)
           "directory" (filetype.directory/contents file-struct files file-list-idx)
           "scss" (filetype.scss/contents file-struct)
           "md"   (filetype.markdown/contents file-struct files file-list-idx)
           "act"  (filetype.act/contents file-struct files file-list-idx)
           "png" (filetype.png/contents file-struct)
           :else (file/read (:source-path file-struct)))))

(defn ->string
  "Serialize a file struct to a string"
  [file-struct]
  (if (:contents file-struct)
    (match (:target-extension file-struct)
      "directory" (filetype.directory/->string file-struct)
      "html" (filetype.html/->string file-struct)
      "css"  (filetype.css/->string file-struct)
      "png"  (filetype.png/->string file-struct)
      :else  (:contents file-struct))
    nil))

(defn ->disk
  "Write a file to its known target path."
  [file-struct]
  (when (:contents file-struct)
    (println "Writing [" (:target-extension file-struct) "] " (:source-path file-struct) " to disk: " (:target-path file-struct))
    (match (:target-extension file-struct)
      "directory" (do (filetype.directory/->disk file-struct)
                      (for [child (:children file-struct)]
                        (->disk child)))
      "html" (filetype.html/->disk file-struct)
      "css"  (filetype.css/->disk file-struct)
      "png"  (filetype.png/->disk file-struct)
      :else  (file/copy (:source-path file-struct)
                        (:target-path file-struct)
                        (:from-dir file-struct)))))
