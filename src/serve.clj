(ns serve
  (:require
   const
   file
   [ring.adapter.jetty :as jetty]
   [file :as file]
   [const :as const]
   [clojure.core.match :refer [match]]))

;; Serve the statically generated files!

(defn get-path [uri]
  (let [path (str const/target-dir uri)]
    (if (file/dir? path)
      (str path "/index.html")
      path)))

(defn content-type [file-path]
  (match (file/extension file-path)
    nil "text/plain"
    "html" "text/html"
    "css" "text/css"
    "js" "text/javascript"))

(defn handler [request]
  (let [file-path (get-path (:uri request))]
    {:status 200
     :headers {"Content-Type" (content-type file-path)}
     :body (file/read file-path)}))

(defn -main [& args]
  (jetty/run-jetty handler {:port const/local-port}))

(comment (-main nil))
