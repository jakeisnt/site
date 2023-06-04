(defn git-history-table
  "Renders the git history for a file given its path."
  [file]
  (let [git-log (git/log (file/path (:source-path file)) (:from-dir file))
        history-link (fn [commit] (git/history-link
                                   (:long-hash commit)
                                   (path/remove-prefix
                                    (:file-path commit)
                                    (:from-dir file))))]
    [:div.git-history-table-container
     [:span.git-history-table-title "Revisions"]
     [:table.git-history-table
      [:tr
       [:th "Date"]
       [:th "Hash"]]
      [:tbody
       (for [commit git-log]
         [:tr
          [:td.commit-date-tr (:commit-date commit)]
          [:td.commit-link-tr [:a {:href (history-link commit)} (:short-hash commit)]]])]]]))

(defn main [file _ __ ___]
  {:depends-on []
   :body  (git-history-table file)})
