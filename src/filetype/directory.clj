(ns filetype.directory
  (:require file html const git path components))

(defn html [source-path target-path files]
  (let [title (file/name source-path)]
    [:html
     (html/head target-path title)
     [:body
      (components/component "sidebar" {:target-path target-path} files nil [[:h1 title]])
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
        (components/component "scroll-up" nil files nil nil)]]]]))

(defn contents [file files _]
  (html (:source-path file) (:target-path file) files))
