(ns components
  (:require
   [hiccup.core :refer [html]]))

(def checkbox-menu
  "An interactive menu that allows the user to optionally enable js features."
  (html
   [:div.checkbox-menu
    [:script :src "/lib.js"]
    [:input
     {:type "checkbox"
      :id "hypothesis-checkbox"
      :checked "false"
      :onclick "toggleHypothesis()"}
     "hypothes.is"]]))

(def link-info
  "A pane to display personal links."
  (html
   [:div.link-info-table
    [:table [:tr
             [:td "Mastodon"]
             [:td [:a :href "https://merveilles.town/@jakeisnt" "jakeisnt"]]]
     [:tr
      [:td "Twitter"]
      [:td [:a :href "https://twitter.com/jakeissnt" "jakeissnt"]]]
     [:tr
      [:td "GitHub"]
      [:td [:a :href "https://github.com/jakeisnt" "jakeisnt"]]]
     [:tr
      [:td "Email"]
      [:td [:a :href "mailto://jake+website@isnt.online" "jake @"]]]]]))

(def js-disabled
  "A pane to display when javascript is disabled."
  [:div.git-hist-table
   [:h3 "JavaScript Disabled"]
   [:p "This website works just fine without JavaScript. Good on you for turning it off."]])

(def lastfm-now-playing
  "
   Displays the track currently playing, or the track that last played, through lastfm.
   NOTE: Currently this is just a hack that strings together some JS files.
         We should make this a proper component and develop an abstraction for interactive components.
  "
  (html
   [:link :rel "stylesheet" :href "/lastfm.css"]
   [:script :src "/lfmNowPlaying.js"]
   [:script "getNowPlaying()"]))

;; (defun file-path-tree (path-parent currently path-children)
;;   "current directory"
;;   ;; "."
;;   ;; "├── "
;;   ;; "└── "

;; TODO: Display some tree of file history with ascii.
;; Show where we are, what's above us, and where we could go, with some context.
;; Use ascii similar to `tree`; should feel like ascii art in a window.
;;
;; IDEA: DSL for describing the folder structure of our repo.
;; This must be a subset of the structure of `wiki`.
;; Files it targets are compiled to their configured locations,
;; but it's easy to understand the sitemap if configured in one place.
;;
;; We should also reuse this framework for headings!
;; A map should be displayed as a tree for every file corresponding to the headings that the file has.

;; TODO
;; (defn lastmod-calendar []
;;   (let ((now (calendar/now)))
;;     (html
;;      [:div.link-info-table
;;       [:span.calendar-title "Ver. " (calendar/month-calendar-title now)]
;;       [calendar::month-html now]])))

;; (defn sidebar
;;   "Display a sidebar for a page, given its root path."
;;   [path root title]

;;   (let ((path (fpath::remove-root path root)))
;;     (spinneret::with-html
;;      (:div
;;       :class "sidebar"
;;       ;; Bold site title instead of making it a clickable link if at root (no path).
;;       (if (endp (fpath::pathdir path))
;;         (:b "jake.")
;;         (:a :href "/index.html" " jake."))
;;       (:a :href "https://isnt.online" " ~ ")
;;       (concatenate
;;        'list
;;        (collect-folder-paths path title)
;;        (list
;;         (:span " / ")
;;         ;; If we have a file title, add and bold it
;;         (when title (:b title))))))))
