(defpackage contract
  (:use :cl))

(in-package :contract)

;; https://github.com/kisom/cl-contracts

(defmacro defcontract (name args arg-contract &body body)
  "Create a function whose arguments will be checked by the arg-contract
and whose output will be checked by out-contract. The contracts should
be functions that do not refer to the names specified in args, and should
return true if the contract has been upheld."
  (let ((result (gensym)))
    `(defun ,name ,args
       ,(unless (null arg-contract)
          `(assert (funcall ,arg-contract ,@args)))
       (let ((,result (progn ,@body)))
         ,(unless t                     ; was out-contreact
            `(assert (funcall ,out-contract ,@args ,result)))
         ,result))))
