(ns serve
  (:require
   const
   file
   path
   clojure.java.io
   [org.httpkit.server :as http]
   [clojure.java.browse :refer [browse-url]]
   [ring.util.mime-type :as mime]
   [juxt.dirwatch :refer [watch-dir]]
   [clojure.string :as str]))

;; Serve the statically generated files!
(defn get-path [uri]
  (let [path (str const/target-dir uri)]
    (if (file/dir? path)
      (str path "/index.html")
      path)))

(defn content-type [file-path]
  (mime/ext-mime-type file-path))

(def html-end-tag "</html>")
(def dev-script "<script type=\"text/javascript\" src=\"/dev-server.js\" id=\"/dev-server.js\"></script>")

(defn inject-hot-reload [site]
  (str/replace-first site html-end-tag (str dev-script html-end-tag)))

(defn handle-file [request]
  (let [file-path (get-path (:uri request))]
    (println "Serving file: " file-path)
    (let [contents (file/read file-path)
          type (content-type file-path)]
      {:status 200
       :headers {"Content-Type" type}
       :body (if (= type "text/html")
               (inject-hot-reload contents)
               contents)})))

(defn handle-socket [request]
  (http/with-channel request channel
    (http/on-receive channel (println "\nSocket opened!"))
    (http/on-close channel (fn [status] (println "Socket closed! " status)))

    ;; if we update a file in /resources,
    ;; we copy it to the target dir so it triggers the build and updates
    (watch-dir
     (fn [event]
       (let [file (:file event)]
         (file/copy file (path/source->target file const/resources-dir const/target-dir))))
     (clojure.java.io/file const/resources-dir))

    (watch-dir
     (fn [event]
       (let [file (path/->url (:file event))]
         ;; event is: {:file name :count file-count :action :create|:modify|:delete}
         (println "File changed: " file)
         (http/send! channel file)))

     (clojure.java.io/file const/target-dir))))

(defn handler [request]
  (if (= (:uri request) "/__hmr")
    (handle-socket request)
    (handle-file request)))

(defn -main [& _]
  (println "Started server")
  (http/run-server handler {:port const/local-port})
  (browse-url (str "http://localhost:" const/local-port)))

(comment (-main nil))
