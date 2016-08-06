#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.convert-functions
  (:use :cl
        :avm
        :avm.lang.syntax
        :avm.lang.built-in)
  (:export :convert-functions))
(in-package :avm.lang.convert-functions)


;;
;; Convert functions

(defun convert-functions (forms)
  (loop for form in forms
     collect (convert-form form)))

(defun convert-form (form)
  (cond
    ((literal-p form) form)
    ((reference-p form) form)
    ((accessor-p form) (convert-accessor form))
    ((the-p form) (convert-the form))
    ((if-p form) (convert-if form))
    ((let-p form) (convert-let form))
    ((flet-p form) (convert-flet form))
    ((labels-p form) (convert-labels form))
    ((set-p form) (convert-set form))
    ((apply-p form) (convert-apply form))
    (t (error "The value ~S is an invalid form." form))))

(defun convert-accessor (form)
  (convert-apply form))

(defun convert-the (form)
  (let ((type (the-type form))
        (value (the-value form)))
    (let ((value1 (convert-form value)))
      `(the ,type ,value1))))

(defun convert-if (form)
  (let ((test-form (if-test-form form))
        (then-form (if-then-form form))
        (else-form (if-else-form form)))
    (let ((test-form1 (convert-form test-form))
          (then-form1 (convert-form then-form))
          (else-form1 (convert-form else-form)))
      `(if ,test-form1 ,then-form1 ,else-form1))))

(defun convert-let (form)
  (let ((bindings (let-bindings form))
        (body (let-body form)))
    (let ((bindings1 (loop for (var value) in bindings
                           for value1 = (convert-form value)
                        collect `(,var ,value1)))
          (body1 (convert-functions body)))
      `(let ,bindings1 ,@body1))))

(defun convert-flet (form)
  (let ((bindings (flet-bindings form))
        (body (flet-body form)))
    (%convert-flet 'flet bindings body)))

(defun %convert-flet (op bindings body)
  (flet ((aux (binding)
           (destructuring-bind (name args . forms) binding
             (let ((args1 (append '(i n) args))
                   (forms1 (convert-functions forms)))
               `(,name ,args1 ,@forms1)))))
    (let ((bindings1 (mapcar #'aux bindings))
          (body1 (convert-functions body)))
      `(,op ,bindings1 ,@body1))))

(defun convert-labels (form)
  (let ((bindings (labels-bindings form))
        (body (labels-body form)))
    (%convert-flet 'labels bindings body)))

(defun convert-set (form)
  (let ((place (set-place form))
        (value (set-value form)))
    (let ((place1 (convert-form place))
          (value1 (convert-form value)))
      `(set ,place1 ,value1))))

(defun convert-apply (form)
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (let ((operands1 (loop for operand in operands
                        collect (convert-form operand))))
      (if (built-in-exists-p operator)
          `(,operator ,@operands1)
          `(,operator i n ,@operands1)))))
