(ns components
  (:require
   html const file path git
   [clojure.core.match :refer [match]]))

;; compile dependencies based on type
(defn make-deps [deps]
  (for [dep deps]
    (match (file/extension (:src dep))
      "js" (html/defer-script (:src dep))
      "css" (html/css (:src dep))
      "scss" (html/css (path/replace-extension (:src dep) "css")))))

;; imports a component and its dependencies where called
(defn component [component-name file files file-list-idx compiled-html]
  (let [component-function
        (load-file
         (str "/home/jake/site/components/" component-name "/" component-name ".clj"))
        cfg (component-function file files file-list-idx compiled-html)
        deps (:depends-on cfg)
        body (:body cfg)]
    [:span body (make-deps deps)]))
