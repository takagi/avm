#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo.lang.convert-functions
  (:use :cl
        :foo
        :foo.lang.type
        :foo.lang.syntax
        :foo.lang.built-in)
  (:export :convert-functions
           :convert-function-name))
(in-package :foo.lang.convert-functions)


;;
;; Convert functions

(defun convert-functions (engine form)
  (cond
    ((literal-p form) form)
    ((reference-p form) form)
    ((accessor-p form) (convert-accessor engine form))
    ((the-p form) (convert-the engine form))
    ((if-p form) (convert-if engine form))
    ((let-p form) (convert-let engine form))
    ((set-p form) (convert-set engine form))
    ((apply-p form) (convert-apply engine form))
    (t (error "The value ~S is an invalid form." form))))

(defun convert-accessor (engine form)
  (convert-apply engine form))

(defun convert-the (engine form)
  (let ((type (the-type form))
        (value (the-value form)))
    (let ((value1 (convert-functions engine value)))
      `(the ,type ,value1))))

(defun convert-if (engine form)
  (let ((test-form (if-test-form form))
        (then-form (if-then-form form))
        (else-form (if-else-form form)))
    (let ((test-form1 (convert-functions engine test-form))
          (then-form1 (convert-functions engine then-form))
          (else-form1 (convert-functions engine else-form)))
      `(if ,test-form1 ,then-form1 ,else-form1))))

(defun convert-let (engine form)
  (flet ((aux (binding)
           (destructuring-bind (var value) binding
             (let ((value1 (convert-functions engine value)))
               `(,var ,value1)))))
    (let ((bindings (let-bindings form))
          (body (let-body form)))
      (let ((bindings1 (mapcar #'aux bindings))
            (body1 (convert-functions engine body)))
        `(let ,bindings1 ,body1)))))

(defun convert-set (engine form)
  (let ((place (set-place form))
        (value (set-value form)))
    (let ((place1 (convert-functions engine place))
          (value1 (convert-functions engine value)))
      `(set ,place1 ,value1))))

(defun convert-apply (engine form)
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (let ((operands1 (loop for operand in operands
                        collect (convert-functions engine operand))))
      (if (built-in-exists-p operator)
          `(,operator ,@operands1)
          (let ((operator1 (convert-function-name operator engine)))
            `(,operator1 i n ,@operands1))))))

(defun convert-function-name (name engine)
  (let ((symbol-name (symbol-name name))
        (symbol-package (symbol-package name)))
    (intern (format nil "%~A-~A" engine symbol-name) symbol-package)))
