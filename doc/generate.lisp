(defpackage :named-values.docs
  (:use #:cl #:gendoc)
  (:export #:generate))

(in-package :named-values.docs)

(defun generate ()
  (gendoc (:output-system :cl-named-values-docs
           :output-filename "index.html"
           :css "simple.css")
    (:mdf "intro.md")
    (:apiref :named-values)))
