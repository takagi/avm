#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.convert-implicit-progn
  (:use :cl
        :avm.lang.syntax)
  (:import-from :alexandria
                :with-gensyms)
  (:export :convert-implicit-progn))
(in-package :avm.lang.convert-implicit-progn)


(defun convert-implicit-progn (forms)
  (destructuring-bind (form1 . forms1) forms
    (if forms1
        (with-gensyms (var)
          `(let ((,var ,(convert-form form1)))
             ,(convert-implicit-progn forms1)))
        (convert-form form1))))

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
    ((setf-p form) (convert-setf form))
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
  (let ((bindings (let-bindings% form))
        (body (let-body% form)))
    (let ((bindings1 (loop for (var value) in bindings
                           for value1 = (convert-form value)
                        collect `(,var ,value1)))
          (body1 (convert-implicit-progn body)))
      `(let ,bindings1 ,body1))))

(defun convert-flet (form)
  (let ((bindings (flet-bindings% form))
        (body (flet-body% form)))
    (%convert-flet 'flet bindings body)))

(defun %convert-flet (op bindings body)
  (let ((bindings1 (loop for (name args . body) in bindings
                         for body1 = (convert-implicit-progn body)
                      collect `(,name ,args ,body1)))
        (body1 (convert-implicit-progn body)))
    `(,op ,bindings1 ,body1)))

(defun convert-labels (form)
  (let ((bindings (labels-bindings% form))
        (body (labels-body% form)))
    (%convert-flet 'labels bindings body)))

(defun convert-setf (form)
  (let ((place (setf-place form))
        (value (setf-value form)))
    (let ((place1 (convert-place place))
          (value1 (convert-form value)))
      `(setf ,place1 ,value1))))

(defun convert-place (place)
  (cond
    ((reference-place-p place) place)
    ((vector-place-p place) (convert-vector-place place))
    ((array-place-p place) (convert-array-place place))
    (t (error "Must not be reached."))))

(defun convert-vector-place (place)
  (let ((operator (vector-place-operator place))
        (value (vector-place-value place)))
    (let ((value1 (convert-place value)))
      `(,operator ,value1))))

(defun convert-array-place (place)
  (let ((value (array-place-value place))
        (index (array-place-index place)))
    (let ((value1 (convert-place value))
          (index1 (convert-form index)))
      `(aref ,value1 ,index1))))

(defun convert-apply (form)
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (let ((operands1 (loop for operand in operands
                        collect (convert-form operand))))
      `(,operator ,@operands1))))
