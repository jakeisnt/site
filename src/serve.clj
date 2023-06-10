;; A very simple static file server

(ns serve
  (:require
   const
   file
   path
   main
   home
   html
   clojure.java.io
   [org.httpkit.server :as http]
   [clojure.java.browse :refer [browse-url]]
   [ring.util.mime-type :as mime]))

;; Serve the statically generated files!
(defn get-path [uri]
  (let [path (str const/target-dir uri)]
    (if (file/dir? path)
      (str path "/index.html")
      path)))

(defn content-type [file-path]
  (mime/ext-mime-type file-path))

(defn handle-file [request]
  (let [file-path (get-path (:uri request))
        type (content-type file-path)
        contents (if (= type "image/png")
                   (file/read-image file-path)
                   (file/read file-path))]
    (println "SERVING [" type "]:" (path/remove-prefix file-path const/target-dir))

    (if contents
      {:status 200
       :headers {"Content-Type" type}
       :body contents}
      {:status 404
       :body "Not found"})))

(defn -main [& _]
  (println "SERVE")
  (http/run-server handle-file {:port const/local-port})
  (println "SOCKET STARTED")
  (browse-url (str "http://localhost:" const/local-port)))

(comment (-main nil))
