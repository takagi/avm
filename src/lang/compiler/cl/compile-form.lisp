#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo.lang.compiler.cl.compile-form
  (:use :cl
        :foo
        :foo.lang.data
        :foo.lang.type
        :foo.lang.syntax
        :foo.lang.built-in
        :foo.lang.typenv
        :foo.lang.compiler.cl.varenv)
  (:export :compile-form))
(in-package :foo.lang.compiler.cl.compile-form)


;;
;; Compile

(defun compile-form (form venv tenv fenv)
  (cond
    ((literal-p form) (compile-literal form venv tenv fenv))
    ((reference-p form) (compile-reference form venv tenv fenv))
    ((accessor-p form) (compile-accessor form venv tenv fenv))
    ((the-p form) (compile-the form venv tenv fenv))
    ((if-p form) (compile-if form venv tenv fenv))
    ((let-p form) (compile-let form venv tenv fenv))
    ((set-p form) (compile-set form venv tenv fenv))
    ((apply-p form) (compile-apply form venv tenv fenv))
    (t (error "The value ~S is an invalid form." form))))

(defun compile-literal (form venv tenv fenv)
  (declare (ignore venv tenv fenv))
  (cond
    ((int-literal-p form) (values form 'int))
    ((float-literal-p form) (values form 'float))
    ((double-literal-p form) (values form 'double))
    (t (error "Must not be reached."))))

(defun compile-reference (form venv tenv fenv)
  (declare (ignore fenv))
  (let* ((type (query-typenv form tenv))
         (form1
          (cond
            ((or (scalar-type-p type)
                 (array-type-p type))
             (if (varenv-exists-p form venv)
                 (query-varenv form venv)
                 `(the ,(compile-type type) ,form)))
            ((vector-type-p type)
             (let ((vars (query-varenv form venv))
                   (vector-values* (vector-type-values* type)))
               `(,vector-values* ,@vars)))
            (t (error "Must not be reached.")))))
    (values form1 type)))

(defun vector-type-values* (type)
  (cl-pattern:match type
    ((:vector 'int 2) 'int2-values*)
    ((:vector 'int 3) 'int3-values*)
    ((:vector 'int 4) 'int4-values*)
    ((:vector 'float 2) 'float2-values*)
    ((:vector 'float 3) 'float3-values*)
    ((:vector 'float 4) 'float4-values*)
    ((:vector 'double 2) 'double2-values*)
    ((:vector 'double 3) 'double3-values*)
    ((:vector 'double 4) 'double4-values*)
    (_ (error "Must not be reached."))))

(defun compile-accessor (form venv tenv fenv)
  (compile-apply form venv tenv fenv))

(defun compile-the (form venv tenv fenv)
  (let ((value (the-value form)))
    (compile-form value venv tenv fenv)))

(defun compile-if (form venv tenv fenv)
  (let ((test-form (if-test-form form))
        (then-form (if-then-form form))
        (else-form (if-else-form form)))
    (multiple-value-bind (test-form1 _)
        (compile-form test-form venv tenv fenv)
      (declare (ignore _))
      (multiple-value-bind (then-form1 _)
          (compile-form then-form venv tenv fenv)
        (declare (ignore _))
        (multiple-value-bind (else-form1 type)
            (compile-form else-form venv tenv fenv)
          (values `(if ,test-form1 ,then-form1 ,else-form1) type))))))

(defun compile-let (form venv tenv fenv)
  (labels ((aux (bindings body venv1 tenv1)
             (if bindings
                 (destructuring-bind ((var value) . bindings1) bindings
                   (multiple-value-bind (value1 type)
                       (compile-form value venv tenv fenv)
                     (let ((tenv2 (extend-typenv var type tenv1)))
                       (multiple-value-bind (venv2 var1)
                           (extend-varenv var type venv1)
                         (cond
                           ((or (scalar-type-p type)
                                (array-type-p type))
                            `(let ((,var1 ,value1))
                               ,(aux bindings1 body venv2 tenv2)))
                           ((vector-type-p type)
                            `(multiple-value-bind ,var1 ,value1
                               ,(aux bindings1 body venv2 tenv2)))
                           (t (error "Must not be reached.")))))))
                 (compile-form body venv1 tenv1 fenv))))
    (let ((bindings (let-bindings form))
          (body (let-body form)))
      (aux bindings body venv tenv))))

(defun compile-set (form venv tenv fenv)
  (let ((place (set-place form))
        (value (set-value form)))
    (multiple-value-bind (place1 type) (compile-form place venv tenv fenv)
      (multiple-value-bind (value1 _) (compile-form value venv tenv fenv)
        (declare (ignore _))
        (values `(setf ,place1 ,value1) type)))))

(defun compile-apply (form venv tenv fenv)
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (let ((argc (built-in-argc operator)))
      (unless (= argc (length operands))
        (error "Invalid number of arguments: ~S" (length operands))))
    (multiple-value-bind (operands1 argtypes)
        (let (operands1 argtypes)
          (loop for operand in operands
             do (multiple-value-bind (operand1 argtype)
                    (compile-form operand venv tenv fenv)
                  (push operand1 operands1)
                  (push argtype argtypes)))
          (values (nreverse operands1) (nreverse argtypes)))
      (let ((operator1 (built-in-operator operator argtypes))
            (type (built-in-return-type operator argtypes)))
        (cond
          ((scalar-type-p type)
           (values `(the ,(compile-type type) (,operator1 ,@operands1)) type))
          ((vector-type-p type)
           (values `(,operator1 ,@operands1) type))
          ((array-type-p type)
           (values `(the ,(compile-type type) (,operator1 ,@operands1)) type))
          (t (error "Must not be reached.")))))))

(defun compile-function-name (name)
  (let ((symbol-name (symbol-name name))
        (symbol-package (symbol-package name)))
    (intern (format nil "%~A" symbol-name) symbol-package)))

(defun compile-type (type)
  (cl-pattern:match type
    ('int 'fixnum)
    ('float 'single-float)
    ('double 'double-float)
    ((:vector _ _) (error "Not implemented."))
    ((:array type1)
     (cl-pattern:match type1
       ('int 'int-array)
       ('float 'float-array)
       ('double 'double-array)
       ((:vector 'int 2) 'int2-array)
       ((:vector 'int 3) 'int3-array)
       ((:vector 'int 4) 'int4-array)
       ((:vector 'float 2) 'float2-array)
       ((:vector 'float 3) 'float3-array)
       ((:vector 'float 4) 'float4-array)
       ((:vector 'double 2) 'double2-array)
       ((:vector 'double 3) 'double3-array)
       ((:vector 'double 4) 'double4-array)
       (_ (error "Must not be reached."))))
    (_ (error "Must not be reached."))))
