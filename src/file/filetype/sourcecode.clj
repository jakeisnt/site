(ns filetype.sourcecode
  (:require file path html git components))

;; if it's a source code file, we want to:
;; - render both to 'file.$ext' and 'file.$ext.html'
;; - hide 'file.$ext' from the index
;; - show 'file.$ext.html' in the index, looking like 'file.$ext'

(defn render-article [md-article file files file-list-idx]
  (let [page-name (:name file)]
    [:html
     (html/head (:target-path file) page-name)
     [:body
      (components/component "sidebar" file files file-list-idx md-article)
      [:div.site-body
       [:main
        [:article.wikipage
         [:h1.title-top page-name]
         md-article]]
       [:div.article-rhs-container
        [:div.article-rhs
         (components/component "git-history-table" file files file-list-idx md-article)
         (components/component "prev-next-up-buttons" file files file-list-idx md-article)]]]]]))

(defn contents [file-obj files file-list-idx]
  (let [file-string (file/read (:source-path file-obj))
        md-article [:pre [:code {:class (str "language-" (:source-extension file-obj) " has-raw-code " (:link file-obj))}
                          file-string]]]
    (render-article md-article file-obj files file-list-idx)))
