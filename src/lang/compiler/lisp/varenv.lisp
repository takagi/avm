#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.compiler.lisp.varenv
  (:use :cl
        :avm.lang.symbol
        :avm.lang.type)
  (:export :empty-varenv
           :extend-varenv
           :varenv-exists-p
           :query-varenv
           :*genvar-counter*
           ))
(in-package :avm.lang.compiler.lisp.varenv)


;;
;; Variable environment

(defun empty-varenv ()
  nil)

(defun extend-varenv (var type venv)
  (check-type var avm-symbol)
  (check-type type avm-type)
  (let ((vars (unique-var var type)))
    (cons (list var vars type) venv)))

(defun varenv-exists-p (var venv)
  (check-type var avm-symbol)
  (and (cdr (assoc var venv))
       t))

(defun query-varenv (var venv)
  (let ((entry (assoc var venv)))
    (if entry
        (values-list (cdr entry))
        (error "The variable ~S not found." var))))

(defvar *genvar-counter* 0)

(defun genvar (var)
  (prog1 (intern (format nil "~A~A" var *genvar-counter*))
    (incf *genvar-counter*)))

(defun unique-var (var type)
  (cond
    ((scalar-type-p type) (list (genvar var)))
    ((vector-type-p type) (loop repeat (vector-type-size type)
                             collect (genvar var)))
    ((array-type-p type) (list (genvar var)))
    (t (error "Must not be reached."))))
