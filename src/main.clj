(ns main
  (:require
   [cybermonday.core :as cm]
   [file :as file]
   [const :as const]
   [html :as html]
   [path :as path]
   [git :as git]))

;; TODO
;; to get up to par with the common lisp build:
;; - implement calendar on home page
;; - parser for scripts/
;; - rewrite file server in clojure

;; To improve:
;; - Journals page

(defn parse-md [file-string]
  (:body (cm/parse-md file-string)))
;; --- Paths ---

;; --- Sidebar ---

;; --- History table ---
(defn history-link
  "Generate a link to git history"
  [long-hash file-path]
  (str const/source-url "/blob/" long-hash "/" file-path))

(defn git-history-table
  "Renders the git history for a file given its path."
  [source-path]
  (let [git-log (git/log (file/path source-path) const/source-dir)]
    [:div.git-hist-table
     [:table
      [:tbody
       (for [commit git-log]
         [:tr
          [:td (:commit-date commit)]
          [:td [:a {:href (history-link (:long-hash commit) (:file-path commit))} (:short-hash commit)]]])]]]))

(defn get-page-name
  "Get the name of the article's page.
  Assume it's the first h1 in the file.
  If unavailable, use the filename."
  [md-article target-path]
  (let [backup (file/name target-path)]
    (if (> (count md-article) 2)
      (let [maybe-title (nth md-article 2)]
        (if (= (first maybe-title) :h1)
          (nth maybe-title 2)
          backup))
      backup)))

(defn has-title?
  "Does the markdown page have a built-in title"
  [md-article]
  (and
   (> (count md-article) 2)
   (> (count (nth md-article 2)) 2)
   (= (first (nth md-article 2)) :h1)))

;; --- index page ---
(defn index-page [source-path target-path]
  (let [title "index"
        files (file/list source-path)]
    (html/->string
     [:html
      (html/head title)
      [:body
       [:div.site-body#site-body
        (html/sidebar target-path title)
        [:main
         [:div.folder-index-page-table
          [:table
           (for [file files]
             (let [log (git/log (file/path file) const/source-dir)]
               [:tr
                [:td (:short-hash (first log))]
                [:td.file-name-tr [:a {:href (path/->url (file/path file))} (file/name file)]]
                [:td.file-type-row (file/extension file)]
                [:td (:commit-date (first log))]]))]]]]]])))

(defn no-js []
  [:noscript
   [:div.git-hist-table
    [:h3 "Thank you for disabling javascript."]
    [:p "This website is augmented with JS, but is perfectly functional without it. The web should be usable with static files alone."]]])

(defn lastfm-now-playing []
  [:span
   [:link {:rel "stylesheet" :href "/lastfm.css"}]
   [:script {:src "/lfmNowPlaying.js"}]
   [:script "getNowPlaying();"]])

(defn link-info-table []
  [:table
   [:tr [:td "Mastodon"] [:td [:a {:href "https://merveilles.town/@jakeisnt"} "jakeisnt"]]]
   [:tr [:td "Twitter"] [:td [:a {:href "https://twitter.com/jakeissnt"} "jakeissnt"]]]
   [:tr [:td "GitHub"] [:td [:a {:href "https://github.com/jakeisnt"} "jakeisnt"]]]
   [:tr [:td "Email"] [:td [:a {:href "mailto://jake+website@isnt.online"} "jake @ ~"]]]])

(defn home-page []
  (html/->string
   [:html
    (html/head "Jake Chvatal")
    [:body
     [:div#site-body.site-body
      (html/sidebar "/" "index")
      [:main
       [:article.wikipage
        [:p "Hey, I'm Jake."]
        [:p "I like old tools and simple ideas."]
        [:p "Check out my " [:a.external {:href "/pages/index.html"} "notes"] " for more."]]
       (no-js)
       (lastfm-now-playing)
       (link-info-table)]]]]))

(defn render-article [md-article source-path target-path]
  (let [page-name (get-page-name md-article target-path)
        has-title (has-title? md-article)]
    (html/->string
     [:html
      (html/head page-name)
      [:body
       [:div.site-body
        (html/sidebar target-path page-name)
        [:main
         [:article.wikipage
          (when (not has-title) [:h1.title-top page-name])
          md-article]]
        (git-history-table source-path)]]])))

(defn transform-file
  "Transform a file from source to target."
  [source-path]
  (let [target-path (path/path-source->target source-path)]
    (->
     source-path
     file/read
     parse-md
     (render-article source-path target-path)
     (file/write target-path))))

(defn make-dir
  "Make a directory listing page"
  [source-path]
  (let [target-path (str (path/path-source->target source-path) "/index.html")]
    (->
     (index-page source-path target-path)
     (file/write target-path))))

(defn -main [_]
  (let [files (file/list const/pages-source-dir)]
    (file/write (home-page) (str const/target-dir "/index.html"))
    (make-dir const/pages-source-dir)
    (doseq [file files]
      (transform-file file))))
