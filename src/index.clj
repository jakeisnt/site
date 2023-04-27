(ns index
  (:require file html const git path components))

(defn get-file-info [file]
  (let [last-log (git/last-log (file/path file) const/source-dir)]
    {:file file :last-log last-log :name (file/name file)}))

;; TODO: goes the wrong way
;; TODO: customize behavior by folder
(defn sort-files [files key]
  (let [file-list (for [file files] (get-file-info file))]
    (reverse (sort-by key file-list))))

(defn html [source-path target-path sort-by]
  (let [title (file/name source-path)
        files (file/list source-path)]
    (html/->string
     [:html
      (html/head target-path title)
      [:body
       (components/sidebar target-path title)
       [:div.site-body
        [:main
         [:div.folder-index-page-table
          [:table
           (for [file (sort-files files sort-by)]
             [:tr
              [:td (:short-hash (:last-log file))]
              [:td.file-name-tr [:a {:href (path/->html (path/->url (file/path (:file file))))} (:name file)]]
              [:td.file-type-row (file/extension (:file file))]
              [:td (:commit-date (:last-log file))]])]]
         (components/component "scroll-up")]]]])))

(defn ->file [source-path target-path key]
  (-> (html source-path target-path key)
      (file/write (str target-path "/index.html"))))
