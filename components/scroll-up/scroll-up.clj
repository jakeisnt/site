(defn main [a b c]
  {:depends-on [{:type :js :src  "/components/scroll-up/scroll-up.js"}
                {:type :css :src "/components/scroll-up/scroll-up.css"}]
   :body [:div.git-hist-table
          [:button.scroll-up-button "Scroll up"]]})
