
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

;; ("p" "filename")
;; ("filename")
;; ("a" "p" "filename")

(defmacro make-pathlist (ls)
  `(loop for pathpart in ls
         collect (:a :href (concatenate 'string root "/" pathpart) )
         )
  )


(defun sidebar (pathlist)
  (let ((root "https://jake.isnt.online"))
    (spinneret::with-html
        (:div
         :class "sidebar"
         ;; (:a :href root "~ ")
         (make-pathlist pathlist)

         ))))
