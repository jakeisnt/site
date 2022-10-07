
(load "~/quicklisp/setup.lisp")
(load "./src/util.lisp")

(ql:quickload :spinneret)
(ql:quickload 'css-lite)
(ql:quickload :parenscript)

(defpackage comp
  (:use :cl))

(in-package :comp)

(defun checkbox-menu ()
  (spinneret::with-html
    (:div :class "checkbox-menu"
          (:input
           :type "checkbox"
           :id "hypothesis-checkbox"
           :checked "false"
           :onclick (parenscript:ps (toggle-hypothesis))
           "hypothes.is"))))

(defun sidebar ()
  (spinneret::with-html
    (:div
     :class "sidebar"
     (:span "~ / ")
     (:a :class "current" :href "https://jake.isnt.online" "home"))))
