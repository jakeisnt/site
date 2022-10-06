(load "~/quicklisp/setup.lisp")

(ql:quickload 'css-lite)

(setf css-lite:*indent-css* 2)

(defpackage css
  (:use :cl :css-lite))

(in-package css)

(defparameter *font-url*
  "@import url('https://fonts.googleapis.com/css2?family=Noto+Sans&display=swap');")

(defparameter *COLOR1* "#484848")
(defparameter *COLOR2* "#585858")

;; DEPRECATED: seems like we can write plain css and it's just, uh, better

(defun style ()
  (concatenate
   'string
   *font-url*
   (css-lite:css
     (("html")
      (:font-size "18px"
       :font-family "\"Noto Sans\", sans-serif"
       :word-wrap "break-word"
       :hyphens "auto")))
   (css-lite:css
     (("body")
      (:max-width "24rem"
       :margin 0
       :padding "1rem 2rem")))
   (css-lite:css
     ((".foot")
      (:padding-right "0.5rem")))
   (css-lite:css
     (("a")
      (("link") (:color *COLOR1*))
      (("visited") (:color *COLOR2*))
      (("active") (:color "blue"))))))

;; (util::write-file "./style.css" *style*)
