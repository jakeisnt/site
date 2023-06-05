;; (:require path file)

;; a component is a clojure file with a main function.
;; the function accepts some 'statically known' information as arguments,
;; returning its dependencies and its body

(defn prev-next-up-buttons  [file-obj files file-list-idx]
  (let [prev-file (and (> file-list-idx 0)
                       (nth files (dec file-list-idx)))
        next-file (and (< file-list-idx (dec (count files)))
                       (nth files (inc file-list-idx)))
        up-link (path/folder (:link file-obj))]
    [:div.prev-next-up-buttons-container
     "Navigation"
     [:table.prev-next-up-buttons
      (when prev-file
        [:tr
         [:td "Previous"]
         [:td [:a.prev-button {:href (:link prev-file)}  (:name prev-file)]]])
      (when next-file
        [:tr
         [:td "Next"]
         [:td [:a.next-button {:href (:link next-file)}  (:name next-file)]]])
      [:tr
       [:td "Up"]
       [:td [:a.up-button {:href up-link} (file/title up-link)]]]]]))

(defn main [file files file-list-idx _]
  {:depends-on []
   :body (prev-next-up-buttons file files file-list-idx)})
