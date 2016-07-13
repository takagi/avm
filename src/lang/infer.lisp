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
        :foo.lang.appenv
        :foo.lang.funenv)
  (:export :infer-function))
(in-package :foo.lang.infer)


;;
;; Type inference

(defun infer-function (name args body tenv aenv uenv fenv &key rec-p)
  (let* ((arg-types (loop for arg in args
                       collect (if (member arg '(i n))
                                   'int (gentype))))
         (return-type (gentype))
         (ftype (make-function-type arg-types return-type)))
    (let ((tenv1 (flet ((aux (tenv pair)
                          (destructuring-bind (arg . type) pair
                            (extend-typenv arg type tenv))))
                   (reduce #'aux (mapcar #'cons args arg-types)
                           :initial-value tenv)))
          (fenv1 (if rec-p
                     (extend-funenv name nil ftype args fenv)
                     fenv)))
      (multiple-value-bind (return-type1 aenv1 uenv1)
          (infer body tenv1 aenv uenv fenv1)
        (multiple-value-bind (_ uenv2) (unify return-type return-type1 uenv1)
          (declare (ignore _))
          (values ftype aenv1 uenv2))))))

(defun infer (form tenv aenv uenv fenv)
  (cond
    ((literal-p form) (infer-literal form tenv aenv uenv fenv))
    ((reference-p form) (infer-reference form tenv aenv uenv fenv))
    ((accessor-p form) (infer-accessor form tenv aenv uenv fenv))
    ((the-p form) (infer-the form tenv aenv uenv fenv))
    ((if-p form) (infer-if form tenv aenv uenv fenv))
    ((let-p form) (infer-let form tenv aenv uenv fenv))
    ((flet-p form) (infer-flet form tenv aenv uenv fenv))
    ((set-p form) (infer-set form tenv aenv uenv fenv))
    ((apply-p form) (infer-apply form tenv aenv uenv fenv))
    (t (error "The value ~S is an invalid form." form))))

(defun infer-literal (form tenv aenv uenv fenv)
  (declare (ignore tenv fenv))
  (cond
    ((int-literal-p form) (values 'int aenv uenv))
    ((float-literal-p form) (values 'float aenv uenv))
    ((double-literal-p form) (values 'double aenv uenv))
    (t (error "Must not be reached."))))

(defun infer-reference (form tenv aenv uenv fenv)
  (declare (ignore fenv))
  (let ((type (query-typenv form tenv)))
    (if type
        (let ((type1 (query-unienv type uenv)))
          (values type1 aenv uenv))
        (error "The variable ~S not found." form))))

(defun infer-accessor (form tenv aenv uenv fenv)
  (infer-apply form tenv aenv uenv fenv))

(defun infer-the (form tenv aenv uenv fenv)
  (let ((type (parse-type (the-type form)))
        (value (the-value form)))
    (multiple-value-bind (type1 aenv1 uenv1) (infer value tenv aenv uenv fenv)
      (multiple-value-bind (type2 uenv2) (unify type type1 uenv1)
        (values type2 aenv1 uenv2)))))

(defun infer-if (form tenv aenv uenv fenv)
  (let ((test-form (if-test-form form))
        (then-form (if-then-form form))
        (else-form (if-else-form form)))
    (multiple-value-bind (test-type1 aenv1 uenv1)
        (infer test-form tenv aenv uenv fenv)
      (multiple-value-bind (_ uenv2)
          (unify test-type1 'bool uenv1)
        (declare (ignore _))
        (multiple-value-bind (then-type1 aenv2 uenv3)
            (infer then-form tenv aenv1 uenv2 fenv)
          (multiple-value-bind (else-type1 aenv3 uenv4)
              (infer else-form tenv aenv2 uenv3 fenv)
            (multiple-value-bind (then-type2 uenv5)
                (unify then-type1 else-type1 uenv4)
              (values then-type2 aenv3 uenv5))))))))

(defun infer-let (form tenv aenv uenv fenv)
  (flet ((aux (tenv-aenv-uenv binding)
           (destructuring-bind (tenv1 aenv1 uenv1) tenv-aenv-uenv
             (destructuring-bind (var value) binding
               (multiple-value-bind (type aenv2 uenv2)
                   (infer value tenv aenv1 uenv1 fenv)
                 (let ((tenv2 (extend-typenv var type tenv1)))
                   (list tenv2 aenv2 uenv2)))))))
    (let ((bindings (let-bindings form))
          (body (let-body form)))
      (destructuring-bind (tenv1 aenv1 uenv1)
          (reduce #'aux bindings :initial-value (list tenv aenv uenv))
        (infer body tenv1 aenv1 uenv1 fenv)))))

(defun infer-flet (form tenv aenv uenv fenv)
  (let ((bindings (flet-bindings form))
        (body (flet-body form)))
    (%infer-flet bindings body tenv aenv uenv fenv fenv)))

(defun %infer-flet (bindings body tenv aenv uenv fenv fenv1)
  (if bindings
      (destructuring-bind ((name args form) . bindings1) bindings
        (multiple-value-bind (ftype aenv1 uenv1)
            (infer-function name args form tenv aenv uenv fenv :rec-p t)
          (let ((fenv2 (extend-funenv name nil ftype args fenv1))
                (aenv2 (extend-appenv (car bindings) ftype aenv1)))
            (%infer-flet bindings1 body tenv aenv2 uenv1 fenv fenv2))))
      (infer body tenv aenv uenv fenv1)))

(defun infer-set (form tenv aenv uenv fenv)
  (let ((place (set-place form))
        (value (set-value form)))
    (multiple-value-bind (type1 aenv1 uenv1) (infer place tenv aenv uenv fenv)
      (multiple-value-bind (type2 aenv2 uenv2)
          (infer value tenv aenv1 uenv1 fenv)
        (multiple-value-bind (type3 uenv3) (unify type1 type2 uenv2)
          (values type3 aenv2 uenv3))))))

(defun infer-apply (form tenv aenv uenv fenv)
  (flet ((aux (aenv-uenv argtype-operand)
           (destructuring-bind (aenv1 uenv1) aenv-uenv
             (destructuring-bind (argtype . operand) argtype-operand
               (multiple-value-bind (type aenv2 uenv2)
                   (infer operand tenv aenv1 uenv1 fenv)
                 (multiple-value-bind (_ uenv3) (unify argtype type uenv2)
                   (declare (ignore _))
                   (list aenv2 uenv3)))))))
    (let ((operator (apply-operator form))
          (operands (apply-operands form)))
      (let ((argc (if (built-in-exists-p operator)
                      (built-in-argc operator)
                      (funenv-argc operator fenv))))
        (unless (= argc (length operands))
          (error "Invalid number of arguments: ~S" (length operands))))
      (let* ((type (if (built-in-exists-p operator)
                      (type-scheme-to-type
                       (built-in-type-scheme operator))
                      (funenv-type operator fenv)))
             (aenv1 (extend-appenv form type aenv)))
        (let ((argtypes (function-arg-types type))
              (return-type (function-return-type type)))
          (destructuring-bind (aenv2 uenv1)
              (reduce #'aux (mapcar #'cons argtypes operands)
                            :initial-value (list aenv1 uenv))
            (let ((return-type1 (query-unienv return-type uenv1)))
              (values return-type1 aenv2 uenv1))))))))
