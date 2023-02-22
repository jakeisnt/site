(ns main
  (:require
   [org-parser.core :refer [read-str]]
   [clj-org.org :refer [parse-org]]
   [hiccup.core :refer [html]]
   [clojure.core.match :refer-macros [match]]
   [clojure.string :as str]
   [clj-jgit.porcelain :as git]))

;; TODO: Why doesn't 'parse' work? Why can't it be found?
;; [org-parser.parser :refer [parse]]

;; TODO
;; (defn -main [opts]
;;   ;; (prn (parse "* Headline"))
;;   (prn (read-str "* Headline"))
;;   (println (write-str (read-str "* Headline"))))

(defn read-file [path]
  (slurp path))

;; 1. Get all files in a directory of the repo
;; 2. Parse each file
;; 3. Re-path each file, get git status and metadata information, etc.
;; 3. Render each file to html

(defn main2 [opts]
  (prn "hello world"))

;; (def my-repo (git/load-repo "/home/jake/site"))
;; (git/git-status my-repo)

(->
 (read-file "/home/jake/site/src/test.org")
 (read-str))

(def file-contents [[:drawer-begin-line [:drawer-name "PROPERTIES"]]
                    [:content ":ID: adsfas\n"]
                    [:drawer-begin-line [:drawer-name "END"]]
                    [:keyword-line [:keyword-key "title"] [:keyword-value "Test"]]
                    [:content "text\n\n"]
                    [:headline {:level 1, :title "bullet"}]
                    [:content "[[https://google.com]]\nThis is a paragraph. It has a link: [[https://google.com][test]]: inside of it.\n"]
                    [:list-item-line [:list-item-bullet "-"] [:list-item-contents "List"]]
                    [:content "  - sublist\n  - sublist\n"]
                    [:list-item-line [:list-item-bullet "-"] [:list-item-contents "parent list"]]
                    [:content "\n"]
                    [:list-item-line [:list-item-counter "1"] [:list-item-counter-suffix "."] [:list-item-contents "bullet 1"]]
                    [:list-item-line [:list-item-counter "2"] [:list-item-counter-suffix "."] [:list-item-contents "wow"]]])

;; - reverse the list
;; - make hierarchical
;; - parse info inside of content blocks
;; -

(defn parse-output [file-contents]
  (reverse (parse-org file-contents)))

(defn parse-keyword [])

;; retrieve id from a content string
(defn parse-id [content-string]
  (-> content-string
      (str/replace #":ID:" "")
      (str/trim)))

;; {:id "asdfas"}
(defn parse-drawer-contents
  ([rst]
   (if (empty rst)
     (cons {} '())
     (let [cur (first rst)
           rst (rest rst)]
       (match cur
         [:content s] (parse-drawer-contents rst {:id (parse-id s)})
         [:drawer-begin-line [:drawer-name "PROPERTIES"]])))))

(defn take-drawer [lst]
  (let [split-idx])

  (split-at split-idx lst))

;; take the drawer from the stream

(defn parse-tok [tok rst]
  (match tok
    [:headline _]
    [:keyword-line _]
    [:drawer-begin-line [:drawer-name "END"]] (parse-drawer-contents rst)
    []))
