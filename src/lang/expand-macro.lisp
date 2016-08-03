#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.expand-macro
  (:use :cl
        :avm.lang.syntax
        :avm.lang.funenv)
  (:export :expand-macro))
(in-package :avm.lang.expand-macro)


(defun expand-macro (form fenv)
  (cond
    ((macro-p form fenv) (expand-macro-macro form fenv))
    ((literal-p form) form)
    ((reference-p form) form)
    ((accessor-p form) (expand-macro-accessor form fenv))
    ((the-p form) (expand-macro-the form fenv))
    ((if-p form) (expand-macro-if form fenv))
    ((let-p form) (expand-macro-let form fenv))
    ((flet-p form) (expand-macro-flet form fenv))
    ((labels-p form) (expand-macro-labels form fenv))
    ((set-p form) (expand-macro-set form fenv))
    ((apply-p form) (expand-macro-apply form fenv))
    (t (error "The value ~S is an invalid form." form))))

(defun macro-p (form fenv)
  (cl-pattern:match form
    ((name . _) (and (atom name)
                     (funenv-macro-exists-p name fenv)))
    (_ nil)))

(defun expand-macro-macro (form fenv)
  (destructuring-bind (name . args) form
    (let ((expander (funenv-macro-expander name fenv)))
      (expand-macro (funcall expander args) fenv))))

(defun expand-macro-accessor (form fenv)
  (expand-macro-apply form fenv))

(defun expand-macro-the (form fenv)
  (let ((type (the-type form))
        (value (the-value form)))
    (let ((value1 (expand-macro value fenv)))
      `(the ,type ,value1))))

(defun expand-macro-if (form fenv)
  (let ((test-form (if-test-form form))
        (then-form (if-then-form form))
        (else-form (if-else-form form)))
    (let ((test-form1 (expand-macro test-form fenv))
          (then-form1 (expand-macro then-form fenv))
          (else-form1 (expand-macro else-form fenv)))
      `(if ,test-form1 ,then-form1 ,else-form1))))

(defun expand-macro-let (form fenv)
  (let ((bindings (let-bindings form))
        (body (let-body form)))
    (let ((bindings1 (loop for (var value) in bindings
                           for value1 = (expand-macro value fenv)
                        collect `(,var ,value1)))
          (body1 (expand-macro body fenv)))
      `(let ,bindings1 ,body1))))

(defun expand-macro-flet (form fenv)
  (let ((bindings (flet-bindings form))
        (body (flet-body form)))
    (%expand-macro-flet 'flet bindings body fenv)))

(defun %expand-macro-flet (op bindings body fenv)
  (let ((bindings1 (loop for (name args body) in bindings
                         for body1 = (expand-macro body fenv)
                      collect `(,name ,args ,body1)))
        (body1 (expand-macro body fenv)))
    `(,op ,bindings1 ,body1)))

(defun expand-macro-labels (form fenv)
  (let ((bindings (labels-bindings form))
        (body (labels-body form)))
    (%expand-macro-flet 'labels bindings body fenv)))

(defun expand-macro-set (form fenv)
  (let ((place (set-place form))
        (value (set-value form)))
    (let ((value1 (expand-macro value fenv)))
      `(set ,place ,value1))))

(defun expand-macro-apply (form fenv)
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (let ((operands1 (loop for operand in operands
                        collect (expand-macro operand fenv))))
      `(,operator ,@operands1))))
