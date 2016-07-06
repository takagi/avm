#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo.lang.compiler.cl.varenv
  (:use :cl
        :foo.lang.symbol
        :foo.lang.type)
  (:export :empty-varenv
           :extend-varenv
           :varenv-exists-p
           :query-varenv
           ))
(in-package :foo.lang.compiler.cl.varenv)


;;
;; Variable environment

(defun empty-varenv ()
  nil)

(defun extend-varenv (var type venv)
  (check-type var foo-symbol)
  (check-type type foo-type)
  (let ((var1 (unique-var var type)))
    (values (acons var var1 venv) var1)))

(defun varenv-exists-p (var venv)
  (check-type var foo-symbol)
  (and (cdr (assoc var venv))
       t))

(defun query-varenv (var venv)
  (or (cdr (assoc var venv))
      (error "The variable ~S not found." var)))

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
