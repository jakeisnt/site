(load "~/quicklisp/setup.lisp")
(load "./parser.lisp")

(ql:quickload :spinneret)
(ql:quickload 'css-lite)
(ql:quickload :parenscript)

(defpackage wiki
  (:use :cl :spinneret))

(defun render-org-node (node)
  "Render an org-mode node as HTML."
  (spinneret::with-html
    (cond
      ((org::header-p node)
       (cons
        (:h2 (org::header-title node))
        (map nil #'render-org-node (org::header-body node))))
      ((org::text-p node) (:p (org::text-body node)))
      ((org::bullet-p node) (:ul (:li (org::bullet-body node))))
      ((org::code-block-p node) (:code (org::code-block-body node))))))

(defun render-org (org)
  "Render an org file struct as an html page"
  (spinneret::with-html
    (:html
     :lang "en-us"
     (:head
      (:title (concatenate 'string (org::file-title org) " | Jake Chvatal")))
     (:body
      (when (org::file-title org)
        (:h1 (org::file-title org)))
      (loop for node in (org::file-body org)
            collect (render-org-node node))))))

(defun render-org-file (fname)
  (render-org (org::parse fname)))
