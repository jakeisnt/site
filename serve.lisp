(load "~/quicklisp/setup.lisp")

(ql:quickload :hunchentoot)

;; A file server for serving static html files in the docs folder.

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))

;; (hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
;;   (setf (hunchentoot:content-type*) "text/plain")
;;   (format nil "Hey~@[ ~A~]!" name))

(setf hunchentoot:*dispatch-table*
      `(hunchentoot:dispatch-easy-handlers
        ,(hunchentoot:create-folder-dispatcher-and-handler "/" "~/site/docs/")))

(defun open-site ()
  "Open a locally hosted dev env for my website."
  (sb-ext:run-program
   "/usr/bin/env"
   (list "bash" "-c" "firefox http://localhost:4242/index.html")
   :input nil
   :output *standard-output*))

(open-site)
