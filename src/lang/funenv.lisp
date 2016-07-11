#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo.lang.funenv
  (:use :cl
        :foo.lang.symbol
        :foo.lang.type)
  (:export :empty-funenv
           :extend-funenv
           :funenv-exists-p
           :funenv-type
           :funenv-arg-types
           :funenv-return-type
           :funenv-arguments
           :funenv-argc))
(in-package :foo.lang.funenv)


;;
;; Function environment

(defun empty-funenv ()
  nil)

(defun extend-funenv (name type arguments fenv)
  (check-type name foo-symbol)
  (loop for type1 in type
     do (check-type type1 foo-type))
  (loop for arg in arguments
     do (check-type arg foo-symbol))
  (assert (= (1- (length type)) (length arguments)))
  (acons name (list type arguments) fenv))

(defun funenv-exists-p (name fenv)
  (check-type name foo-symbol)
  (and (cdr (assoc name fenv))
       t))

(defun funenv-type (name fenv)
  (or (cadr (assoc name fenv))
      (error "The function ~S not defined." name)))

(defun funenv-arg-types (name fenv)
  (function-arg-types (funenv-type name fenv)))

(defun funenv-return-type (name fenv)
  (function-return-type (funenv-type name fenv)))

(defun funenv-arguments (name fenv)
  (or (caddr (assoc name fenv))
      (error "The function ~S not defined." name)))

(defun funenv-argc (name fenv)
  (length (funenv-arguments name fenv)))
