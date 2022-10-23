(load "~/quicklisp/setup.lisp")

(ql:quickload :cl-ppcre)

(defpackage git
  (:use :cl :cl-ppcre))

(in-package :git)

(defun get-created-dt (src-path)
  "Get the date at which an article was created."
  (with-output-to-string (*dt-stream*)
    (sb-ext:run-program
     "/usr/bin/env"
     (list "bash" "-c"
           (concatenate
            'string
            "cd /home/jake/wiki && git log --follow --format=%ad --date default --date=format:'%Y-%m-%d' " (namestring src-path) " | tail -1"))
     :input nil
     :output *dt-stream*)))

(defun log-of-file (src-path)
  "
   Given a source path of a file in /home/jake/wiki,
   produces a list of `((commit-hash modification-time) ...),
   with the most recent modification time first.
   Format string: `(short-hash long-hash date), producing list of these for every file.
   NOTE: %s is commit message, but i don't want to worry about parsing it rn.
  "
  (let ((res
          (with-output-to-string (*dt-stream*)
            (sb-ext:run-program
             "/usr/bin/env"
             (list
              "bash" "-c"
              (concatenate
               'string
               "cd /home/jake/wiki && git log --all --pretty=\"format:%h %H %ad\" --date default --date=format:'%Y-%m-%d' " (namestring src-path)))
             :input nil
             :output *dt-stream*))))
    (loop for line in (split #\newline res)
          collect (split #\space line))))
