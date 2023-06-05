(ns filetype.directory
  (:require html file components))

(defn html [file-obj files]
  (let [title (:name file-obj)]
    [:html
     (html/head (:target-path file-obj) title)
     [:body
      (components/component "sidebar" file-obj files nil [[:h1 title]])
      [:div.site-body
       [:main
        [:div.folder-index-page-table
         [:table
          (for [child-file files]
            [:tr
             [:td.file-hash-tr (:short-hash (:last-log child-file))]
             [:td.file-name-tr [:a {:href (:link child-file)} (:name child-file)]]
             [:td.file-type-tr (:source-extension child-file)]
             [:td.file-date-tr (:commit-date (:last-log child-file))]])]]
        (components/component "scroll-up" file-obj files nil nil)]]]]))

(defn contents [file-info files _]
  (html file-info files))

(defn ->string [file-obj]
  (html/->string (:contents file-obj)))

(defn ->disk [file-obj]
  (file/make-directory (:target-dir file-obj))
  (file/write (:target-path file-obj) (->string file-obj)))
