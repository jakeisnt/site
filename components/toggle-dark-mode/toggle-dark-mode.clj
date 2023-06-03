(defn main [file files file-list-idx]
  {:depends-on [{:type :js :src  "/components/toggle-dark-mode/toggle-dark-mode.js"}
                {:type :css :src "/components/toggle-dark-mode/toggle-dark-mode.css"}]
   :body [:div.toggle-dark-mode-container
          [:button.toggle-dark-mode ""]]})
