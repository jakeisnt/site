(ns index
  (:require file html const git path))

(defn html [source-path target-path]
  (let [title (file/name source-path)
        files (file/list source-path)]
    (html/->string
     [:html
      (html/head target-path title)
      [:body
       [:div.site-body
        (html/sidebar target-path title)
        [:main
         [:div.folder-index-page-table
          [:table
           (for [file files]
             (let [log (git/log (file/path file) const/source-dir)]
               [:tr
                [:td (:short-hash (first log))]
                [:td.file-name-tr [:a {:href (path/->html (path/->url (file/path file)))} (file/name file)]]
                [:td.file-type-row (file/extension file)]
                [:td (:commit-date (first log))]]))]]]]]])))

(defn ->file [source-path target-path]
  (-> (html source-path target-path)
      (file/write (str target-path "/index.html"))))
