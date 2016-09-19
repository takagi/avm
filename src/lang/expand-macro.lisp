#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.expand-macro
  (:use :cl
        :avm.lang.symbol
        :avm.lang.syntax
        :avm.lang.funenv)
  (:export :expand-macro))
(in-package :avm.lang.expand-macro)


(defun macro-p (form fenv)
  (cl-pattern:match form
    ((name . _) (and (avm-symbol-p name)
                     (funenv-macro-exists-p name fenv)))
    (_ nil)))

(defun expand-macro (forms fenv)
  (loop for form in forms
     collect (expand-macro-form form fenv)))

(defun expand-macro-form (form fenv)
  (cond
    ((macro-p form fenv) (expand-macro-macro form fenv))
    ((literal-p form) (expand-macro-literal form fenv))
    ((reference-p form) (expand-macro-reference form fenv))
    ((accessor-p form) (expand-macro-accessor form fenv))
    ((the-p form) (expand-macro-the form fenv))
    ((if-p form) (expand-macro-if form fenv))
    ((let-p form) (expand-macro-let form fenv))
    ((flet-p form) (expand-macro-flet form fenv))
    ((labels-p form) (expand-macro-labels form fenv))
    ((setf-p form) (expand-macro-setf form fenv))
    ((apply-p form) (expand-macro-apply form fenv))
    (t (error "The value ~S is an invalid form." form))))

(defun expand-macro-macro (form fenv)
  (destructuring-bind (name . args) form
    (let ((expander (funenv-macro-expander name fenv)))
      (expand-macro-form (funcall expander args) fenv))))

(defun expand-macro-literal (form fenv)
  (declare (ignore fenv))
  form)

(defun expand-macro-reference (form fenv)
  (declare (ignore fenv))
  form)

(defun expand-macro-accessor (form fenv)
  (expand-macro-apply form fenv))

(defun expand-macro-the (form fenv)
  (let ((type (the-type form))
        (value (the-value form)))
    (let ((value1 (expand-macro-form value fenv)))
      `(the ,type ,value1))))

(defun expand-macro-if (form fenv)
  (let ((test-form (if-test-form form))
        (then-form (if-then-form form))
        (else-form (if-else-form form)))
    (let ((test-form1 (expand-macro-form test-form fenv))
          (then-form1 (expand-macro-form then-form fenv))
          (else-form1 (expand-macro-form else-form fenv)))
      `(if ,test-form1 ,then-form1 ,else-form1))))

(defun expand-macro-let (form fenv)
  (let ((bindings (let-bindings% form))
        (body (let-body% form)))
    (let ((bindings1 (loop for (var value) in bindings
                           for value1 = (expand-macro-form value fenv)
                        collect `(,var ,value1)))
          (body1 (expand-macro body fenv)))
      `(let ,bindings1 ,@body1))))

(defun expand-macro-flet (form fenv)
  (let ((bindings (flet-bindings% form))
        (body (flet-body% form)))
    (%expand-macro-flet 'flet bindings body fenv)))

(defun %expand-macro-flet (op bindings body fenv)
  (let ((bindings1 (loop for (name args . body) in bindings
                         for body1 = (expand-macro body fenv)
                      collect `(,name ,args ,@body1)))
        (body1 (expand-macro body fenv)))
    `(,op ,bindings1 ,@body1)))

(defun expand-macro-labels (form fenv)
  (let ((bindings (labels-bindings% form))
        (body (labels-body% form)))
    (%expand-macro-flet 'labels bindings body fenv)))

(defun expand-macro-setf (form fenv)
  (let ((place (setf-place form))
        (value (setf-value form)))
    (let ((place1 (expand-macro-place place fenv))
          (value1 (expand-macro-form value fenv)))
      `(setf ,place1 ,value1))))

(defun expand-macro-place (place fenv)
  (cond
    ((reference-place-p place) (expand-macro-reference place fenv))
    ((vector-place-p place) (expand-macro-vector-place place fenv))
    ((array-place-p place) (expand-macro-array-place place fenv))
    (t (error "Must not be reached."))))

(defun expand-macro-vector-place (place fenv)
  (let ((operator (vector-place-operator place))
        (value (vector-place-value place)))
    (let ((value1 (expand-macro-place value fenv)))
      `(,operator ,value1))))

(defun expand-macro-array-place (place fenv)
  (let ((value (array-place-value place))
        (index (array-place-index place)))
    (let ((value1 (expand-macro-place value fenv))
          (index1 (expand-macro-form index fenv)))
      `(aref ,value1 ,index1))))

(defun expand-macro-apply (form fenv)
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (let ((operands1 (loop for operand in operands
                        collect (expand-macro-form operand fenv))))
      `(,operator ,@operands1))))
