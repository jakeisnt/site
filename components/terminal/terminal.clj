(defn main [a b c]
  {:depends-on [{:src "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.4/jquery.js"}
                {:src "https://cdn.jsdelivr.net/gh/jcubic/jquery.terminal@devel/js/jquery.terminal.min.js"}
                {:src "https://cdn.jsdelivr.net/gh/jcubic/jquery.terminal@devel/css/jquery.terminal.min.css"}
                {:src "/components/terminal/terminal.js"}
                {:src "/components/terminal/terminal.css"}]
   :body [:div.terminal.git-hist-table {:id "terminal"}]})
