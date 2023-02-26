(ns serve
  (:require
   const
   file
   [ring.adapter.jetty :as jetty]
   [ring.util.mime-type :as mime]))

;; Serve the statically generated files!
(defn get-path [uri]
  (let [path (str const/target-dir uri)]
    (if (file/dir? path)
      (str path "/index.html")
      path)))

(defn content-type [file-path]
  (mime/ext-mime-type file-path))

(defn handler [request]
  (println "Requesting path: " (:uri request))
  (let [file-path (get-path (:uri request))]
    {:status 200
     :headers {"Content-Type" (content-type file-path)}
     :body (file/read file-path)}))

(defn -main [& args]
  (jetty/run-jetty handler {:port const/local-port}))

(comment (-main nil))
