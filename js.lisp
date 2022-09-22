(load "~/quicklisp/setup.lisp")

(ql:quickload :parenscript)

;; i want common lisp to have some traversable evaluation tree
;; in which i can view previous or future states of the editor before and after
;; evaluation in an immutable fashion. that would be cool.

(defpackage ps-code
  (:use :cl :parenscript))  ;; :cl :parenscript (should be identical lol)

(in-package ps-code)

(with-open-file (str "./lib.js"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (format
   str
   (ps
     ;; dynamically load documentation from a script,
     ;; associating it with an id
     (defun dyn-load (src id)
       (let ((s (chain document (create-element "script"))))
         (chain console (log "i am hypothesis"))
         (chain s (set-attribute "src" src))
         (chain s (set-attribute "id" id))
         (chain s (set-attribute "async" t))
         s))

     ;; load the hypothesis browser extension
     (defun load-hypothesis ()
       (chain
        document
        body
        (append-child
         (dyn-load "https://hypothes.is/embed.js" "hypothesis"))))

     ;; unload the hypothesis browser extension
     (defun unload-hypothesis ()
       (let ((annotator-link (chain document (query-selector "link[type=\"application/annotator+html\"]"))))
         (if annotator-link
             (let ((destroy-event (new (-Event "destroy")))) (chain annotator-link (dispatch-event destroy-event))))))

     (load-hypothesis))))
