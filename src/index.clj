(ns index
  (:require file html const git path))

(defn get-file-info [file]
  (let [last-log (git/last-log (file/path file) const/source-dir)]
    {:file file :last-log last-log :name (file/name file)}))

;; TODO: goes the wrong way
;; TODO: customize behavior by folder
(defn sort-files [files]
  (let [file-list (for [file files] (get-file-info file))]
    (sort-by :name file-list)))

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
           (for [file (sort-files files)]
             [:tr
              [:td (:short-hash (:last-log file))]
              [:td.file-name-tr [:a {:href (path/->html (path/->url (file/path (:file file))))} (:name file)]]
              [:td.file-type-row (file/extension (:file file))]
              [:td (:commit-date (:last-log file))]])]]]]]])))

(defn ->file [source-path target-path]
  (-> (html source-path target-path)
      (file/write (str target-path "/index.html"))))
