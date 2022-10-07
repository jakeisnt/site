(defpackage fp
  (:use :cl))

(in-package :fp)

;; a path is a list of folders
;; as well as a file name and a file extension
(defstruct filepath dirs name type)

(defun make-pathlist (current-path newtype)
  (make-filepath
   :dirs (list "p")
   :name (pathname-name current-path)
   :type newtype))

(defun change-file-path (current-path dir root)
  "Change a file path to a new name."
  (concatenate
   'string
   root
   dir
   (pathname-name current-path)
   ".html"))
