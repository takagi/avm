#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.funenv
  (:use :cl
        :avm.lang.symbol
        :avm.lang.type)
  (:export :empty-funenv
           ;; Function
           :extend-funenv-function
           :funenv-function-exists-p
           :funenv-function-name
           :funenv-function-name1
           :funenv-function-type
           :funenv-function-arg-types
           :funenv-function-return-type
           :funenv-function-arguments
           :funenv-function-argc
           ;; Macro
           :extend-funenv-macro
           :funenv-macro-exists-p
           :funenv-macro-name
           :funenv-macro-arguments
           :funenv-macro-body
           :funenv-macro-expander))
(in-package :avm.lang.funenv)


;;
;; Function environment

(defun empty-funenv ()
  nil)


;;
;; Function environment - function

(defun extend-funenv-function (name name1 type args fenv)
  (check-type name avm-symbol)
  (check-type name1 symbol)
  (check-type type function-type)
  (loop for arg in args
     do (check-type arg avm-symbol))
  (assert (= (1- (length type)) (length args)))
  (cons (list name :function name1 type args) fenv))

(defun funenv-function-exists-p (name fenv)
  (check-type name avm-symbol)
  (let ((entry (assoc name fenv)))
    (and entry
         (eq (second entry) :function))))

(defun %lookup-function (name fenv)
  (if (funenv-function-exists-p name fenv)
      (assoc name fenv)
      (error "The function ~S is undefined." name)))

(defun funenv-function-name (name fenv)
  (first (%lookup-function name fenv)))

(defun funenv-function-name1 (name fenv)
  (third (%lookup-function name fenv)))

(defun funenv-function-type (name fenv)
  (fourth (%lookup-function name fenv)))

(defun funenv-function-arg-types (name fenv)
  (function-arg-types (funenv-function-type name fenv)))

(defun funenv-function-return-type (name fenv)
  (function-return-type (funenv-function-type name fenv)))

(defun funenv-function-arguments (name fenv)
  (fifth (%lookup-function name fenv)))

(defun funenv-function-argc (name fenv)
  (length (funenv-function-arguments name fenv)))


;;
;; Function environment - macro

(defun extend-funenv-macro (name args body expander fenv)
  (check-type name avm-symbol)
  (check-type body list)
  (check-type expander function)
  (cons (list name :macro args body expander) fenv))

(defun funenv-macro-exists-p (name fenv)
  (check-type name avm-symbol)
  (let ((entry (assoc name fenv)))
    (and entry
         (eq (second entry) :macro))))

(defun %lookup-macro (name fenv)
  (if (funenv-macro-exists-p name fenv)
      (assoc name fenv)
      (error "The macro ~S is undefined." name)))

(defun funenv-macro-name (name fenv)
  (first (%lookup-macro name fenv)))

(defun funenv-macro-arguments (name fenv)
  (third (%lookup-macro name fenv)))

(defun funenv-macro-body (name fenv)
  (fourth (%lookup-macro name fenv)))

(defun funenv-macro-expander (name fenv)
  (fifth (%lookup-macro name fenv)))
