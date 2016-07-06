#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo.lang.built-in
  (:use :cl
        :foo)
  (:export :built-in-functions
           :built-in-arithmetic-p
           :built-in-arithmetic-left-assoc-p
           :built-in-arithmetic-right-assoc-p
           :built-in-type-scheme
           :built-in-argc
           :built-in-operator
           :built-in-return-type
           ))
(in-package :foo.lang.built-in)


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

(defun built-in-type-scheme (name)
  (cons :type-scheme
   (or (cadr (assoc name (built-in-functions :cl)))
       (error "The function ~S is not defined." name))))

(defun built-in-argc (name)
  (- (length (built-in-type-scheme name)) 2))

(defun built-in-candidates (name)
  (or (caddr (assoc name (built-in-functions :cl)))
      (error "The function ~S is not defined." name)))

(defun built-in-elected (name argtypes)
  (or (assoc argtypes (built-in-candidates name)
             :key #'butlast :test #'equal)
      (error "The function ~S is not defined." name)))

(defun built-in-operator (name argtypes)
  (cadr (built-in-elected name argtypes)))

(defun built-in-return-type (name argtypes)
  (type-return-type
   (car (built-in-elected name argtypes))))

(defun type-arg-types (type)
  (butlast type))

(defun type-return-type (type)
  (car (last type)))
