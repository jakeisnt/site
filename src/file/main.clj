(ns filetype.main
  (:require
   filetype.directory filetype.scss filetype.markdown filetype.act filetype.html filetype.css filetype.png filetype.sourcecode
   file path git
   [clojure.core.match :refer [match]]))

(defn info
  "Provided a source file path, its dir, and a target dir,
    assemble a map of information about the file."
  [file-obj source-dir target-dir root-target]
  (let [file-path (file/path file-obj)
        last-log (git/last-log file-path source-dir)
        is-directory (file/dir? file-path)
        directory-ty (if is-directory "dir" nil)
        src-extension (or (file/extension file-path) directory-ty nil)
        target-extension (filetype.main/target-extension src-extension)
        target-path (path/swapext
                     (path/source->target file-path source-dir target-dir)
                     target-extension)
        name (file/name file-obj)
        link (path/remove-prefix target-path root-target)
        show-source-view (show-source-view? src-extension)]

    {:from-dir source-dir
     :source-path file-path
     :target-path target-path
     :target-dir target-dir
     :is-directory is-directory
     :source-extension src-extension
     :target-extension target-extension
     :link link
     :last-log last-log
     :name name

     ;; render a nice view of the source code in addition to the file's normal target
     :show-source-view show-source-view
     :view-link (str link ".html")
     :view-target-path (str target-path ".html")
     :view-extension "html"}))
