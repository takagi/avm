#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.built-in
  (:use :cl
        :avm
        :avm.lang.type)
  (:export :built-in-functions
           :built-in-arithmetic-p
           :built-in-arithmetic-left-assoc-p
           :built-in-arithmetic-right-assoc-p
           :built-in-exists-p
           :built-in-type-scheme
           :built-in-argc
           :built-in-operator
           ))
(in-package :avm.lang.built-in)


;;
;; Built-in functions

(defgeneric built-in-functions (engine))

(defun built-in-arithmetic-p (name)
  (and (member name '(+ - * / *. .* /.))
       t))

(defun built-in-arithmetic-left-assoc-p (name)
  (and (built-in-arithmetic-p name)
       (not (built-in-arithmetic-right-assoc-p name))))

(defun built-in-arithmetic-right-assoc-p (name)
  (eq name '.*))

(defun built-in-exists-p (name)
  (and (assoc name (built-in-functions :cl))
       t))

(defun built-in-type-scheme (name)
  (cons :type-scheme
   (or (cadr (assoc name (built-in-functions :cl)))
       (error "The function ~S is not defined." name))))

(defun built-in-argc (name)
  (- (length (built-in-type-scheme name)) 2))

(defun built-in-candidates (name)
  (or (caddr (assoc name (built-in-functions :cl)))
      (error "The function ~S is not defined." name)))

(defun built-in-elected (name function-type)
  (or (assoc function-type (built-in-candidates name) :test #'equal)
      (error "The function ~S is not defined." name)))

(defun built-in-operator (name function-type)
  (cadr (built-in-elected name function-type)))
