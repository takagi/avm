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

(defun convert-functions (form)
  (cond
    ((literal-p form) form)
    ((reference-p form) form)
    ((accessor-p form) (convert-accessor form))
    ((the-p form) (convert-the form))
    ((if-p form) (convert-if form))
    ((let-p form) (convert-let form))
    ((flet-p form) (convert-flet form))
    ((labels-p form) (convert-labels form))
    ((setf-p form) (convert-setf form))
    ((apply-p form) (convert-apply form))
    (t (error "The value ~S is an invalid form." form))))

(defun convert-accessor (form)
  (convert-apply form))

(defun convert-the (form)
  (let ((type (the-type form))
        (value (the-value form)))
    (let ((value1 (convert-functions value)))
      `(the ,type ,value1))))

(defun convert-if (form)
  (let ((test-form (if-test-form form))
        (then-form (if-then-form form))
        (else-form (if-else-form form)))
    (let ((test-form1 (convert-functions test-form))
          (then-form1 (convert-functions then-form))
          (else-form1 (convert-functions else-form)))
      `(if ,test-form1 ,then-form1 ,else-form1))))

(defun convert-let (form)
  (flet ((aux (binding)
           (destructuring-bind (var value) binding
             (let ((value1 (convert-functions value)))
               `(,var ,value1)))))
    (let ((bindings (let-bindings form))
          (body (let-body form)))
      (let ((bindings1 (mapcar #'aux bindings))
            (body1 (convert-functions body)))
        `(let ,bindings1 ,body1)))))

(defun convert-flet (form)
  (let ((bindings (flet-bindings form))
        (body (flet-body form)))
    (%convert-flet 'flet bindings body)))

(defun %convert-flet (op bindings body)
  (flet ((aux (binding)
           (destructuring-bind (name args body) binding
             (let ((args1 (append '(i n) args))
                   (body1 (convert-functions body)))
               `(,name ,args1 ,body1)))))
    (let ((bindings1 (mapcar #'aux bindings))
          (body1 (convert-functions body)))
      `(,op ,bindings1 ,body1))))

(defun convert-labels (form)
  (let ((bindings (labels-bindings form))
        (body (labels-body form)))
    (%convert-flet 'labels bindings body)))

(defun convert-setf (form)
  (let ((place (setf-place form))
        (value (setf-value form)))
    (let ((place1 (convert-functions place))
          (value1 (convert-functions value)))
      `(setf ,place1 ,value1))))

(defun convert-apply (form)
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (let ((operands1 (loop for operand in operands
                        collect (convert-functions operand))))
      (if (built-in-exists-p operator)
          `(,operator ,@operands1)
          `(,operator i n ,@operands1)))))
