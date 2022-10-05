(load "~/quicklisp/setup.lisp")
(load "./util.lisp")

(ql:quickload 'css-lite)

(setf css-lite:*indent-css* 2)

(defparameter *font-url*
  "@import url('https://fonts.googleapis.com/css2?family=Noto+Sans&display=swap');")

(defparameter *style*
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
      (:padding-right "0.5rem")))))

(util::write-file "./style.css" *style*)
