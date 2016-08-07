#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.kernel
  (:use :cl
        :avm
        :avm.lang.symbol
        :avm.lang.type)
  (:shadow :function)
  (:import-from :alexandria
                :with-gensyms)
  (:export ;; Kernel
           :kernel
           :make-kernel
           :kernel-function-names
           :kernel-macro-names
           ;; Functions
           :kernel-define-function
           :kernel-function-exists-p
           :kernel-function-name
           :kernel-function-lisp-name
           :kernel-function-type
           :kernel-function-arguments
           :kernel-function-body
           ;; Macros
           :kernel-define-macro
           :kernel-macro-exists-p
           :kernel-macro-name
           :kernel-macro-arguments
           :kernel-macro-body
           :kernel-macro-expander
           ;; Globals
           ;; Constants
           ;; Symbol macros.
           ))
(in-package :avm.lang.kernel)


;;
;; Functions

(defstruct (function (:constructor %make-function))
  (name :name :read-only t)
  (lisp-name :lisp-name :read-only t)
  (type :type :read-only t)
  (arguments :arguments :read-only t)
  (body :body :read-only t))

(defun make-function (name lisp-name type arguments body)
  (check-type name avm-symbol)
  (check-type lisp-name symbol)
  (check-type type function-type)
  (loop for argument in arguments
     do (check-type argument avm-symbol))
  (check-type body list)
  (unless (= (1- (length type)) (length arguments))
    (error "Invalid number of arguments against type: ~S" (length arguments)))
  (%make-function :name name
                  :lisp-name lisp-name
                  :type type
                  :arguments arguments
                  :body body))


;;
;; Macros

(defstruct (macro (:constructor %make-macro))
  (name :name :read-only t)
  (arguments :arguments :read-only t)
  (body :body :read-only t)
  (expander :expander :read-only t))

(defun make-macro (name arguments body)
  (check-type name avm-symbol)
  (with-gensyms (arguments1)
    (let ((expander (eval `#'(lambda (,arguments1)
                               (destructuring-bind ,arguments ,arguments1
                                 ,@body)))))
      (%make-macro :name name
                   :arguments arguments
                   :body body
                   :expander expander))))


;;
;; Globals

(defstruct (global (:constructor %make-global))
  (name :name :read-only t)
  (value :value :read-only t))

(defun make-global (name value)
  (check-type name avm-symbol)
  (%make-global :name name :value value))


;;
;; Constants

(defstruct (constant (:constructor %make-constant))
  (name :name :read-only t)
  (value :value :read-only t))

(defun make-constant (name value)
  (check-type name avm-symbol)
  (%make-constant :name name :value value))


;;
;; Symbol macros

(defstruct (symbol-macro (:constructor %make-symbol-macro))
  (name :name :read-only t)
  (expansion :expansion :read-only t))

(defun make-symbol-macro (name expansion)
  (check-type name avm-symbol)
  (%make-symbol-macro :name name :expansion expansion))


;;
;; Kernel

(defstruct (kernel (:constructor %make-kernel))
  (variable-namespace :variable-namespace)
  (function-namespace :function-namespace))

(defun make-kernel ()
  (%make-kernel :variable-namespace '()
                :function-namespace '()))

(defun kernel-function-names (kernel)
  (let ((namespace (kernel-function-namespace kernel)))
    (nreverse
     (loop for (name entry) on namespace by #'cddr
        when (function-p entry)
        collect name))))

(defun kernel-macro-names (kernel)
  (let ((namespace (kernel-function-namespace kernel)))
    (nreverse
     (loop for (name entry) on namespace by #'cddr
        when (macro-p entry)
        collect name))))


;;
;; Kernel - Functions

(defun kernel-define-function (kernel name lisp-name type args body)
  (symbol-macrolet ((namespace (kernel-function-namespace kernel)))
    (let ((function (make-function name lisp-name type args body)))
      (setf (getf namespace name) function)))
  name)

(defun kernel-function-exists-p (kernel name)
  (let ((namespace (kernel-function-namespace kernel)))
    (function-p (getf namespace name))))

(defun %lookup-function (kernel name)
  (unless (kernel-function-exists-p kernel name)
    (error "The function ~S is undefined." name))
  (let ((namespace (kernel-function-namespace kernel)))
    (getf namespace name)))

(defun kernel-function-name (kernel name)
  (function-name (%lookup-function kernel name)))

(defun kernel-function-lisp-name (kernel name)
  (function-lisp-name (%lookup-function kernel name)))

(defun kernel-function-type (kernel name)
  (function-type (%lookup-function kernel name)))

(defun kernel-function-arguments (kernel name)
  (function-arguments (%lookup-function kernel name)))

(defun kernel-function-body (kernel name)
  (function-body (%lookup-function kernel name)))


;;
;; Kernel - Macros

(defun kernel-define-macro (kernel name args body)
  (symbol-macrolet ((namespace (kernel-function-namespace kernel)))
    (let ((macro (make-macro name args body)))
      (setf (getf namespace name) macro)))
  name)

(defun kernel-macro-exists-p (kernel name)
  (let ((namespace (kernel-function-namespace kernel)))
    (macro-p (getf namespace name))))

(defun %lookup-macro (kernel name)
  (unless (kernel-macro-exists-p kernel name)
    (error "The macro ~S is undefined." name))
  (let ((namespace (kernel-function-namespace kernel)))
    (getf namespace name)))

(defun kernel-macro-name (kernel name)
  (macro-name (%lookup-macro kernel name)))

(defun kernel-macro-arguments (kernel name)
  (macro-arguments (%lookup-macro kernel name)))

(defun kernel-macro-body (kernel name)
  (macro-body (%lookup-macro kernel name)))

(defun kernel-macro-expander (kernel name)
  (macro-expander (%lookup-macro kernel name)))


;;
;; Kernel - Globals


;;
;; Kernel - Constants


;;
;; Kernel - Symbol Macros


