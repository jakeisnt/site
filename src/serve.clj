(ns serve
  (:require
   const file path main home html clojure.java.io
   [org.httpkit.server :as http]
   [clojure.java.browse :refer [browse-url]]
   [ring.util.mime-type :as mime]
   [juxt.dirwatch :refer [watch-dir]]
   [clojure.string :as str]))

;; Serve the statically generated files!
(defn get-path [uri target-dir]
  ;; TODO: replace overlap between target-dir and path
  (let [path (str target-dir uri)]
    (if (file/dir? path)
      (str path "/index.html")
      path)))

(defn content-type [file-path]
  (mime/ext-mime-type file-path))

(def html-end-tag "</html>")
(def dev-script "<script type=\"text/javascript\" src=\"/dev-server.js\" id=\"/dev-server.js\"></script>")

(defn inject-hot-reload [site]
  (str/replace-first site html-end-tag (str dev-script html-end-tag)))

(defn serve-file [request source-dir target-dir]
  (let [file-path (get-path (:uri request) target-dir)
        type (content-type file-path)
        contents (if (= type "image/png")
                   (file/read-image file-path)
                   (file/read file-path))]
    (println "SERVING [" type "]:" (path/remove-prefix file-path target-dir))
    {:status 200
     :headers {"Content-Type" type}
     :body (if (= type "text/html")
             (inject-hot-reload contents)
             contents)}))

;; recompile a file when it's modified
(defn recompile-on-change [source-dir target-dir]
  (watch-dir
   (fn [event]
     (let [file (:file event)]
       (main/compile-file source-dir target-dir file [] nil true)))
   (clojure.java.io/file source-dir)))

;;  if any of the dependencies of 'home' are modified,
;;  we rebuild the home page
;;  TODO generalize
;;  TODO deps are determined statically?
;;  TODO lots of fun things
(defn watch-home [target-dir]
  (let [home-page (home/home)
        deps (:depends-on home-page)]
    (doseq [dep-name deps]
      (watch-dir
       (fn [event]
         (println "Home page deps changed, rebuilding home page:" dep-name)
         (file/write (html/->string (:body (home/home))) (str target-dir "/index.html")))
       (clojure.java.io/file (str "/home/jake/site/components" "/" dep-name))))))

(defn handle-socket [request source-dir target-dir]
  (http/with-channel request channel
    (http/on-receive channel (println "\n--- SOCKET CONNECTED ---"))
    (http/on-close channel (fn [status] (println "--- SOCKET DISCONNECTED --- " status)))

    (watch-dir
     (fn [event]
       (let [file
             (path/swapext (path/remove-prefix (:file event) target-dir)
                           (main/get-target-extension (:file event)))]
         ;; event is: {:file name :count file-count :action :create|:modify|:delete}
         (println "CHANGED: " file)
         (http/send! channel file)))

     (clojure.java.io/file target-dir))))

(defn handler [request source-dir target-dir]
  (if (= (:uri request) "/__hmr")
    (handle-socket request source-dir target-dir)
    (serve-file request source-dir target-dir)))

(defn -main [& _]
  (let [source-dir "/home/jake/site"
        target-dir "/home/jake/site/docs"
        local-port 4242]
    (println "SERVE")

    (main/-main)
    (http/run-server (fn [req] (handler req source-dir target-dir)) {:port local-port})
    (println "SOCKET STARTED")

    (doseq [path const/site-paths]
      (recompile-on-change (str source-dir "/" (:folder path)) target-dir))

    (watch-home target-dir)
    (browse-url (str "http://localhost:" local-port))))

(comment (-main nil))
