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
     (defun print (txt)
       (chain console (log txt)))

     ;; dynamically load documentation from a script,
     ;; associating it with an id
     (defun dyn-load (src id)
       (let ((s (chain document (create-element "script"))))
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

     (defun hypothesis-annotator-link ()
       (chain document (query-selector "link[type=\"application/annotator+html\"]")))

     ;; unload the hypothesis browser extension
     (defun unload-hypothesis (annotator-link)
       (if annotator-link
           (let ((destroy-event (new (-Event "destroy")))) (chain annotator-link (dispatch-event destroy-event)))))

     ;; toggle the hypothesis extension on or off
     (defun toggle-hypothesis ()
       (let ((annotator-link (hypothesis-annotator-link)))
         (if annotator-link
             (unload-hypothesis annotator-link)
             (load-hypothesis))))

     ;; respect the current state of the checkbox when loading hypothes.is
     (let ((hypothesis-doc (chain document (get-element-by-id "hypothesis-checkbox"))))
       (if (chain hypothesis-doc checked) (load-hypothesis))))))
