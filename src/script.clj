(ns script
  (:require
   [file :as file]
   [instaparse.core :as insta]
   [clojure.core.match :refer [match]]
   [html :as html]
   [path :as path]
   [const :as const]))

(def parse
  (insta/parser "
<S> = TITLE,ALIAS*,(SUBTEXT | LINE | COMMENT | NEWLINE)+
TITLE = HASH,STRING,NEWLINE
ALIAS = ALIAS_NAME,EQUAL,ALIAS_NAME,NEWLINE
SUBTEXT = OPEN_PAREN,STRING,CLOSE_PAREN,NEWLINE
LINE = ALIAS_NAME,COLON,STRING,NEWLINE
COMMENT = DASH,STRING,NEWLINE

DASH = '-'
HASH = '#'
OPEN_PAREN = '('
CLOSE_PAREN = ')'
EQUAL = '='
COLON=': '
ALIAS_NAME=#'[a-zA-Z0-9]+'
NEWLINE='\n'
STRING=#'[^()\n]+'
"))

(defn parse-script [str]
  (parse str))

(defn include-in-script? [line]
  (and
   (not= :NEWLINE (first line))
   (not= :COMMENT (first line))))

(defn third [v]
  (nth v 2))

(defn fourth [v]
  (nth v 3))

(defn script-title [script]
  [(second (third (first script)))
   (rest script)])

(defn script-alias [alias]
  (let [name (second (fourth alias))
        as (second (second alias))]
    {:name name :as as}))

(defn script-aliases [script]
  (let [[aliases rst] (split-with #(= :ALIAS (first %)) (rest script))
        aliases (map script-alias aliases)]
    [aliases rst]))

(defn script-body [script]
  (for [line script]
    (match line
      [:LINE
       [:ALIAS_NAME author]
       [:COLON ": "]
       [:STRING message]
       [:NEWLINE "\n"]] [:line author message]
      [:SUBTEXT
       [:OPEN_PAREN "("]
       [:STRING content]
       [:CLOSE_PAREN ")"]
       [:NEWLINE "\n"]] [:context content])))

(defn ->ast [parsed]
  (let [script (filter include-in-script? parsed)
        [title script] (script-title script)
        [aliases script] (script-aliases script)
        body (script-body script)]
    ;; switch(node):
    ;; - title -> add to title of doc
    ;; - alias -> add to alias map
    ;; - subtext -> add inline to document as subtext
    ;; - line: add linline as a line.
    {:title title :aliases aliases :body body}))

(defn character-selector [script]
  [:div.script-control-menu
   [:p "You're acting as"]
   [:select {:name "characters" :id "characterSelector"}
    (for [alias (:aliases script)]
      [:option.character {:value (:name alias)} (:name alias)])]
   " ."])

(defn first-author?
  "Is the author provided the first author of the script?"
  [script author]
  (= author (first (:name  (:aliases script)))))

(defn message-direction [script author]
  (if (first-author? script author)
    "right"
    "left"))

(defn line->html [line script]
  (match line
    [:line author message]
    [:blockquote {:class (str "message " author " " (message-direction script author))}
     [:div {class (str "message-body " author " " (message-direction script author))}
      [:p.message-text [:span message]]]]

    [:context content]
    [:p.context content]))

(defn ->html
  "Convert the script to an HTML document."
  [script path]
  [:html
   (html/head (:title script))
   [:body
    [:div.site-body#site-body
     (html/sidebar path (:title script))
     [:main
      [:div
       [:link {:rel "stylesheet" :href "/conversation.css"}]
       (character-selector script)
       [:article.conversation
        (for [line (:body script)]
          (line->html line script))]]]
     [:script {:src "/script.js"}]]]])

(defn ->file [source-path]
  (let [target-path (path/path-source->target source-path)]
    (->
     path
     file/read
     parse-script
     ->ast
     (->html path)
     html/->string
     (file/write target-path))))

;; NOTE:
;; The regexes are not perfect
;; Disallow `:`, `=`, `(`, `)` in str and alias names to avoid ambiguity.
