(ns home
  (:require
   [hiccup.core :refer [html]]))

(defn index []
  (html [:html
         [:head
          [:title "Jake Chvatal"]]
         [:body
          [:h1 "Hello World"]]]))
