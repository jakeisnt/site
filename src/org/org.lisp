
(load "~/quicklisp/setup.lisp")
(load "~/site/src/org/ast.lisp")
(load "~/site/src/org/lexer.lisp")
(load "~/site/src/org/parser.lisp")
(load "~/site/src/org/html.lisp")

(defpackage org
  (:use :cl))

(in-package org)

(defun parse (fstream)
  (parser::parse fstream))

(defun html (fdata path root extras)
  (org-html::render-file fdata path root extras))

(defun p (data)
  "Is the file an org-ast?"
  (ast::file-p data))

(defun filename (data)
  "What's the name of this file?"
  (ast::file-title data))