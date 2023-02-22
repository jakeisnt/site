(ns main
  (:require [org-parser.parser :refer [parse]]
            [org-parser.core :refer [read-str write-str]]))

(defn run [opts]
  (prn (parse "* Headline"))
  (prn (read-str "* Headline"))
  (println (write-str (read-str "* Headline"))))
