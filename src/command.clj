(ns command
  (:require
   [clojure.string :as str]
   [const :as const]
   [file :as file]
   [clojure.java.shell :as sh]))

;; clojure git configuration
;; https://gist.github.com/rnesytov/b944bf9f681b0c42519cd0b6ab8f44e5
(defn run [command & {:keys [directory] :or {directory (System/getProperty "user.dir")}}]
  (sh/with-sh-dir directory
    (sh/sh "sh" "-c" command)))

(defn res [command & [args]]
  (reduce
   (fn [command arg-key]
     (str/replace command (str "%" (name arg-key)) (arg-key args)))
   command
   (keys args)))

(defn exec
  "
    Executes a command with arguments, producing
    the output of the command if successful and false otherwise.
  "
  [command path & [args]]
  (let [result (run (res command args) :directory path)]
    (if (zero? (:exit result))
      (:out result)
      false)))
