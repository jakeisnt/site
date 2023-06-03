(ns index
  (:require file html const git path components))

(defn html [source-path target-path files]
  (let [title (file/name source-path)]
    [:html
     (html/head target-path title)
     [:body
      (components/sidebar target-path title)
      [:div.site-body
       [:main
        [:div.folder-index-page-table
         [:table
          (for [file files]
            [:tr
             [:td.file-hash-tr (:short-hash (:last-log file))]
             [:td.file-name-tr [:a {:href (:link file)} (:name file)]]
             [:td.file-type-tr (file/extension (:file file))]
             [:td.file-date-tr (:commit-date (:last-log file))]])]]
        (components/component "scroll-up" files nil nil)]]]]))

(defn ->file [source-path target-path key]
  (let [contents (html source-path target-path key)]
    (file/write (html/->string contents) (str target-path "/index.html"))
    contents))
