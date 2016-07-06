#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo.lang.infer
  (:use :cl
        :foo
        :foo.lang.type
        :foo.lang.syntax
        :foo.lang.built-in
        :foo.lang.unienv
        :foo.lang.typenv
        :foo.lang.funenv)
  (:export :infer))
(in-package :foo.lang.infer)


;;
;; Type inference

(defun infer (form tenv uenv fenv)
  (cond
    ((literal-p form) (infer-literal form tenv uenv fenv))
    ((reference-p form) (infer-reference form tenv uenv fenv))
    ((accessor-p form) (infer-accessor form tenv uenv fenv))
    ((the-p form) (infer-the form tenv uenv fenv))
    ((if-p form) (infer-if form tenv uenv fenv))
    ((let-p form) (infer-let form tenv uenv fenv))
    ((set-p form) (infer-set form tenv uenv fenv))
    ((apply-p form) (infer-apply form tenv uenv fenv))
    (t (error "The value ~S is an invalid form." form))))

(defun infer-literal (form tenv uenv fenv)
  (declare (ignore tenv fenv))
  (cond
    ((int-literal-p form) (values 'int uenv))
    ((float-literal-p form) (values 'float uenv))
    ((double-literal-p form) (values 'double uenv))
    (t (error "Must not be reached."))))

(defun infer-reference (form tenv uenv fenv)
  (declare (ignore fenv))
  (let ((type (query-typenv form tenv)))
    (if type
        (let ((type1 (query-unienv type uenv)))
          (values type1 uenv))
        (error "The variable ~S not found." form))))

(defun infer-accessor (form tenv uenv fenv)
  (infer-apply form tenv uenv fenv))

(defun infer-the (form tenv uenv fenv)
  (let ((type (parse-type (the-type form)))
        (value (the-value form)))
    (multiple-value-bind (type1 uenv1) (infer value tenv uenv fenv)
      (unify type type1 uenv1))))

(defun infer-if (form tenv uenv fenv)
  (let ((test-form (if-test-form form))
        (then-form (if-then-form form))
        (else-form (if-else-form form)))
    (multiple-value-bind (type1 uenv1) (infer test-form tenv uenv fenv)
      (multiple-value-bind (_ uenv2) (unify type1 'bool uenv1)
        (declare (ignore _))
        (multiple-value-bind (type3 uenv3) (infer then-form tenv uenv2 fenv)
          (multiple-value-bind (type4 uenv4) (infer else-form tenv uenv3 fenv)
            (unify type3 type4 uenv4)))))))

(defun infer-let (form tenv uenv fenv)
  (flet ((aux (tenv-uenv binding)
           (destructuring-bind (tenv1 . uenv1) tenv-uenv
             (destructuring-bind (var value) binding
               (multiple-value-bind (type uenv2) (infer value tenv uenv1 fenv)
                 (let ((tenv2 (extend-typenv var type tenv1)))
                   (cons tenv2 uenv2)))))))
    (let ((bindings (let-bindings form))
          (body (let-body form)))
      (destructuring-bind (tenv1 . uenv1)
          (reduce #'aux bindings :initial-value (cons tenv uenv))
        (infer body tenv1 uenv1 fenv)))))

(defun infer-set (form tenv uenv fenv)
  (let ((place (set-place form))
        (value (set-value form)))
    (multiple-value-bind (type1 uenv1) (infer place tenv uenv fenv)
      (multiple-value-bind (type2 uenv2) (infer value tenv uenv1 fenv)
        (unify type1 type2 uenv2)))))

(defun infer-apply (form tenv uenv fenv)
  (flet ((aux (uenv1 argtype-operand)
           (destructuring-bind (argtype . operand) argtype-operand
             (multiple-value-bind (type uenv2) (infer operand tenv uenv1 fenv)
               (multiple-value-bind (_ uenv3) (unify argtype type uenv2)
                 (declare (ignore _))
                 uenv3)))))
    (let ((operator (apply-operator form))
          (operands (apply-operands form)))
      (let ((argc (if (built-in-exists-p operator)
                      (built-in-argc operator)
                      (funenv-argc operator fenv))))
        (unless (= argc (length operands))
          (error "Invalid number of arguments: ~S" (length operands))))
      (let ((type (if (built-in-exists-p operator)
                      (type-scheme-to-type
                       (built-in-type-scheme operator))
                      (funenv-type operator fenv))))
        (let ((argtypes (type-arg-types type))
              (return-type (type-return-type type)))
          (let* ((uenv1 (reduce #'aux (mapcar #'cons argtypes operands)
                                :initial-value uenv))
                 (type1 (query-unienv return-type uenv1)))
            (values type1 uenv1)))))))

(defun type-arg-types (type)
  (butlast type))

(defun type-return-type (type)
  (car (last type)))
