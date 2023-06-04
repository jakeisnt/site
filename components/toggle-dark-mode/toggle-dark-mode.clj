(defn main [file files file-list-idx _]
  {:depends-on [{:type :js :src  "/components/toggle-dark-mode/toggle-dark-mode.js"}
                {:type :css :src "/components/toggle-dark-mode/toggle-dark-mode.css"}]
   :body [:div.toggle-dark-mode-container
          [:button.toggle-dark-mode ""]]})
