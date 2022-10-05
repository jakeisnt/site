(load "~/quicklisp/setup.lisp")
(load "./util.lisp")
(load "./homepage.lisp")

(ql:quickload :spinneret)

(util::write-file
 "./index.html"
 (spinneret::with-html-string (homepage::homepage)))
