;; generate a map of a page with links to content
(defn page-map [compiled-html file]
  (let [tags (html/find-tags compiled-html :h1 :h2 :h3 :h4 :h5 :h6)
        path-link (:link file)]
    [:div.sitemap-container
     [:span.sitemap-title "In this article"]
     [:table.sitemap
      (map (fn [tag]
             (let [tag-id (:id (second tag))
                   tag-text (or
                             (and (> 2 (count tag)) (nth tag 2))
                             tag-id)]
               [:tr
                [:td {:id (str "page-map-" tag-id)} [:a {:href (str path-link "#" tag-id)} tag-text]]]))
           tags)]]))

(defn main [file files file-idx compiled-html]
  {:depends-on []
   :body (page-map compiled-html file)})
