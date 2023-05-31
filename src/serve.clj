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
  (let [file-path (get-path (:uri request))
        type (content-type file-path)
        contents (if (= type "image/png")
                   (file/read-image file-path)
                   (file/read file-path))]
    (println "SERVING [" type "]:" (path/remove-prefix file-path const/target-dir))
    {:status 200
     :headers {"Content-Type" type}
     :body (if (= type "text/html")
             (inject-hot-reload contents)
             contents)}))

;; if we update a file in /resources,
;; we copy it to the target dir so it triggers the build and updates
(defn watch-resources []
  (watch-dir
   (fn [event]
     (let [file (:file event)]
       (println "Copying resources file" file)
       (file/copy file (path/source->target file const/resources-dir const/target-dir))))
   (clojure.java.io/file const/resources-dir)))

;; we also copy the files of the components dir
;; (TODO: more build system logic to come)
(defn watch-components []
  (watch-dir
   (fn [event]
     (let [file (:file event)]
       (println "Copying component file" file "to " (path/source->target file const/components-dir const/target-dir))
       (file/copy file (path/source->target file const/current-repo const/target-dir))))
   (clojure.java.io/file const/components-dir)))

;;  if any of the dependencies of 'home' are modified,
;;  we rebuild the home page
;;  TODO generalize
;;  TODO deps are determined statically?
;;  TODO lots of fun things
(defn watch-home []
  (let [home-page (home/home)
        deps (:depends-on home-page)]
    (doseq [dep-name deps]
      (watch-dir
       (fn [event]
         (println "Home page deps changed, rebuilding home page:" dep-name)
         (file/write (html/->string (:body (home/home))) (str const/target-dir "/index.html")))
       (clojure.java.io/file (str const/components-dir "/" dep-name))))))

(defn handle-socket [request]
  (http/with-channel request channel
    (http/on-receive channel (println "\n--- SOCKET CONNECTED ---"))
    (http/on-close channel (fn [status] (println "--- SOCKET DISCONNECTED --- " status)))

    (watch-dir
     (fn [event]
       (let [file (path/->url (:file event))]
         ;; event is: {:file name :count file-count :action :create|:modify|:delete}
         (println "CHANGED: " file)
         (http/send! channel file)))

     (clojure.java.io/file const/target-dir))))

(defn handler [request]
  (if (= (:uri request) "/__hmr")
    (handle-socket request)
    (handle-file request)))

(defn -main [& _]
  (println "SERVE")
  (main/copy-resources)
  (http/run-server handler {:port const/local-port})
  (println "SOCKET STARTED")

  (watch-resources)
  (watch-components)
  (watch-home)

  (browse-url (str "http://localhost:" const/local-port)))

(comment (-main nil))
