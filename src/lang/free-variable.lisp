#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.free-variable
  (:use :cl
        :avm
        :avm.lang.syntax)
  (:export :check-free-variable))
(in-package :avm.lang.free-variable)


;;
;; Check free variable existence

(defun check-free-variable (args form vars)
  (let ((vars1 (append args vars)))
    (%check-free-variable form vars vars1))
  form)

(defun %check-free-variable (form vars0 vars)
  (cond
    ((literal-p form) nil)
    ((reference-p form) (check-free-variable-reference form vars0 vars))
    ((accessor-p form) (check-free-variable-accessor form vars0 vars))
    ((the-p form) (check-free-variable-the form vars0 vars))
    ((if-p form) (check-free-variable-if form vars0 vars))
    ((let-p form) (check-free-variable-let form vars0 vars))
    ((flet-p form) (check-free-variable-flet form vars0 vars))
    ((labels-p form) (check-free-variable-labels form vars0 vars))
    ((setf-p form) (check-free-variable-setf form vars0 vars))
    ((apply-p form) (check-free-variable-apply form vars0 vars))
    (t (error "The value ~S is an invalid form." form))))

(defun check-free-variable-reference (form vars0 vars)
  (declare (ignore vars0))
  (or (member form vars)
      (error "The variable ~S not found." form)))

(defun check-free-variable-accessor (form vars0 vars)
  (check-free-variable-apply form vars0 vars))

(defun check-free-variable-the (form vars0 vars)
  (let ((value (the-value form)))
    (%check-free-variable value vars0 vars)))

(defun check-free-variable-if (form vars0 vars)
  (let ((test-form (if-test-form form))
        (then-form (if-then-form form))
        (else-form (if-else-form form)))
    (%check-free-variable test-form vars0 vars)
    (%check-free-variable then-form vars0 vars)
    (%check-free-variable else-form vars0 vars)))

(defun check-free-variable-let (form vars0 vars)
  (flet ((aux (vars1 binding)
           (destructuring-bind (var value) binding
             (%check-free-variable value vars0 vars)
             (cons var vars1))))
    (let ((bindings (let-bindings form))
          (body (let-body form)))
      (let ((vars1 (reduce #'aux bindings :initial-value vars)))
        (%check-free-variable body vars0 vars1)))))

(defun check-free-variable-flet (form vars0 vars)
  (let ((bindings (flet-bindings form))
        (body (flet-body form)))
    (%check-free-variable-flet bindings body vars0 vars)))

(defun %check-free-variable-flet (bindings body vars0 vars)
  (flet ((aux (binding)
           (destructuring-bind (name args body) binding
             (declare (ignore name))
             (check-free-variable args body vars0))))
    (loop for binding in bindings
       do (aux binding))
    (%check-free-variable body vars0 vars)))

(defun check-free-variable-labels (form vars0 vars)
  (let ((bindings (labels-bindings form))
        (body (labels-body form)))
    (%check-free-variable-flet bindings body vars0 vars)))

(defun check-free-variable-setf (form vars0 vars)
  (let ((place (setf-place form))
        (value (setf-value form)))
    (%check-free-variable place vars0 vars)
    (%check-free-variable value vars0 vars)))

(defun check-free-variable-apply (form vars0 vars)
  (let ((operands (apply-operands form)))
    (loop for operand in operands
       do (%check-free-variable operand vars0 vars))))
