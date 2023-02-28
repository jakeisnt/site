(ns serve
  (:require
   const
   file
   path
   clojure.java.io
   [org.httpkit.server :as http]
   ;; [ring.adapter.jetty :as jetty]
   [ring.util.mime-type :as mime]
   [juxt.dirwatch :refer [watch-dir]]))

;; Serve the statically generated files!
(defn get-path [uri]
  (let [path (str const/target-dir uri)]
    (if (file/dir? path)
      (str path "/index.html")
      path)))

(defn content-type [file-path]
  (mime/ext-mime-type file-path))

(defn handle-file [request]
  (let [file-path (get-path (:uri request))]
    (println "Serving file " file-path)
    {:status 200
     :headers {"Content-Type" (content-type file-path)}
     :body (file/read file-path)}))

(defn handle-socket [request]
  (http/with-channel request channel
    (http/on-close channel (fn [status] (println "Socket closed! " status)))
    (watch-dir
     (fn [event]
       (let [file (path/->url (:file event))]
         ;; event is: {:file name :count file-count :action :create|:modify|:delete}
         (println "file changed: " file)
         (http/send! channel file)))

     (clojure.java.io/file const/target-dir))))

(defn handler [request]
  (println "Requesting path: " (:uri request))
  (println (:uri request))
  (if (= (:uri request) "/__hmr")
    (handle-socket request)
    (handle-file request)))

(defn -main [& _]
  (println "Started server")
  (http/run-server handler {:port const/local-port}))

(comment (-main nil))
