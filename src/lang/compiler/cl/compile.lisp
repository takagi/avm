#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo.lang.compiler.cl.compile
  (:use :cl
        :foo
        :foo.lang.data
        :foo.lang.type
        :foo.lang.syntax
        :foo.lang.built-in
        :foo.lang.typenv
        :foo.lang.appenv
        :foo.lang.funenv
        :foo.lang.compiler.cl.compile-type
        :foo.lang.compiler.cl.varenv)
  (:export :compile-function))
(in-package :foo.lang.compiler.cl.compile)


;;
;; Genname

(defvar *genname-counter* 0)

(defun genname (name)
  (prog1 (intern (format nil "~A~A" name *genname-counter*))
    (incf *genname-counter*)))


;;
;; Compile

(defun compile-function (name ftype args body venv tenv aenv fenv
                         &key entry-p rec-p)
  (assert (not (and entry-p (not rec-p))))
  (let* ((name1 (compile-name name entry-p))
         (pairs (loop for arg in args
                      for type in (function-arg-types ftype)
                     collect (cons arg type)))
         (venv1 (flet ((aux (venv pair)
                         (destructuring-bind (arg . type) pair
                           (extend-varenv arg type venv))))
                  (reduce #'aux pairs :initial-value venv)))
         (tenv1 (flet ((aux (tenv pair)
                         (destructuring-bind (arg . type) pair
                           (extend-typenv arg type tenv))))
                  (reduce #'aux pairs :initial-value tenv)))
         (fenv1 (if rec-p
                    (extend-funenv name name1 ftype args fenv)
                    fenv)))
    (let ((args1 (loop for arg in args
                    append (query-varenv arg venv1)))
          (types1 (loop for type in (function-arg-types ftype)
                     collect (compile-type type)))
          (body1 (compile-form body venv1 tenv1 aenv fenv1)))
      (values name1 args1
              `((declare (optimize (speed 3) (safety 0)))
                (declare (ignorable ,@args1))
                ,@(loop for arg1 in args1
                        for type1 in types1
                     collect `(declare (type ,type1 ,arg1)))
                ,body1)))))

(defun compile-name (name entry-p)
  (if entry-p
      (let ((symbol-name (symbol-name name))
            (symbol-package (symbol-package name)))
        (intern (format nil "%CL-~A" symbol-name) symbol-package))
      (genname name)))

(defun compile-form (form venv tenv aenv fenv)
  (cond
    ((literal-p form) (compile-literal form venv tenv aenv fenv))
    ((reference-p form) (compile-reference form venv tenv aenv fenv))
    ((accessor-p form) (compile-accessor form venv tenv aenv fenv))
    ((the-p form) (compile-the form venv tenv aenv fenv))
    ((if-p form) (compile-if form venv tenv aenv fenv))
    ((let-p form) (compile-let form venv tenv aenv fenv))
    ((labels-p form) (compile-labels form venv tenv aenv fenv))
    ((flet-p form) (compile-flet form venv tenv aenv fenv))
    ((set-p form) (compile-set form venv tenv aenv fenv))
    ((apply-p form) (compile-apply form venv tenv aenv fenv))
    (t (error "The value ~S is an invalid form." form))))

(defun compile-literal (form venv tenv aenv fenv)
  (declare (ignore venv tenv aenv fenv))
  (cond
    ((int-literal-p form) (values form 'int))
    ((float-literal-p form) (values form 'float))
    ((double-literal-p form) (values form 'double))
    (t (error "Must not be reached."))))

(defun compile-reference (form venv tenv aenv fenv)
  (declare (ignore aenv fenv))
  (let ((type (query-typenv form tenv)))
    (cond
      ((scalar-type-p type)
       (let ((form1 (car (query-varenv form venv))))
         (values form1 type)))
      ((vector-type-p type)
       (let* ((vars (query-varenv form venv))
              (vector-values* (vector-type-values* type))
              (form1 `(,vector-values* ,@vars)))
         (values form1 type)))
      ((array-type-p type)
       (let ((form1 (car (query-varenv form venv))))
         (values form1 type)))
      (t (error "Must not be reached.")))))

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

(defun compile-accessor (form venv tenv aenv fenv)
  (compile-apply form venv tenv aenv fenv))

(defun compile-the (form venv tenv aenv fenv)
  (let ((value (the-value form)))
    (compile-form value venv tenv aenv fenv)))

(defun compile-if (form venv tenv aenv fenv)
  (let ((test-form (if-test-form form))
        (then-form (if-then-form form))
        (else-form (if-else-form form)))
    (multiple-value-bind (test-form1 _)
        (compile-form test-form venv tenv aenv fenv)
      (declare (ignore _))
      (multiple-value-bind (then-form1 _)
          (compile-form then-form venv tenv aenv fenv)
        (declare (ignore _))
        (multiple-value-bind (else-form1 type)
            (compile-form else-form venv tenv aenv fenv)
          (values `(if ,test-form1 ,then-form1 ,else-form1) type))))))

(defun compile-let (form venv tenv aenv fenv)
  (let ((bindings (let-bindings form))
        (body (let-body form)))
    (%compile-let bindings body venv tenv aenv fenv venv tenv)))

(defun %compile-let (bindings body venv tenv aenv fenv venv1 tenv1)
  (if bindings
      (destructuring-bind ((var value) . bindings1) bindings
        (multiple-value-bind (value1 type)
            (compile-form value venv tenv aenv fenv)
          (let ((tenv2 (extend-typenv var type tenv1)))
            (multiple-value-bind (venv2 vars) (extend-varenv var type venv1)
              (cond
                ;; Scalar type and array type.
                ((or (scalar-type-p type)
                     (array-type-p type))
                 (let ((type1 (compile-type type)))
                   `(let ((,@vars ,value1))
                      (declare (type ,type1 ,@vars))
                      ,(%compile-let bindings1 body
                                     venv tenv aenv fenv venv2 tenv2))))
                ;; Vector type.
                ((vector-type-p type)
                 (let ((type1 (compile-type (vector-type-base-type type))))
                   `(multiple-value-bind ,vars ,value1
                      ,@(loop for var1 in vars
                           collect
                             `(declare (type ,type1 ,var1)))
                      ,(%compile-let bindings1 body venv
                                     tenv aenv fenv venv2 tenv2))))
                (t (error "Must not be reached.")))))))
      (compile-form body venv1 tenv1 aenv fenv)))

(defun compile-flet (form venv tenv aenv fenv)
  (let ((bindings (flet-bindings form))
        (body (flet-body form)))
    (%compile-flet 'flet bindings body nil venv tenv aenv fenv fenv)))

(defun %compile-flet (op bindings body rec-p venv tenv aenv fenv fenv1)
  (if bindings
      (destructuring-bind ((name args form) . bindings1) bindings
        (let ((ftype (query-appenv (car bindings) aenv)))
          (multiple-value-bind (name1 args1 form1)
              (compile-function name ftype args form venv tenv aenv fenv
                                :entry-p nil :rec-p rec-p)
            (let ((fenv2 (extend-funenv name name1 ftype args fenv1)))
              (multiple-value-bind (body1 return-type)
                  (%compile-flet op bindings1 body rec-p
                                 venv tenv aenv fenv fenv2)
                (values `(,op ((,name1 ,args1 ,@form1))
                           ,body1)
                        return-type))))))
      (compile-form body venv tenv aenv fenv1)))

(defun compile-labels (form venv tenv aenv fenv)
  (let ((bindings (labels-bindings form))
        (body (labels-body form)))
    (%compile-flet 'labels bindings body t venv tenv aenv fenv fenv)))

(defun compile-set (form venv tenv aenv fenv)
  (let ((place (set-place form))
        (value (set-value form)))
    (multiple-value-bind (place1 type) (compile-form place venv tenv aenv fenv)
      (multiple-value-bind (value1 _) (compile-form value venv tenv aenv fenv)
        (declare (ignore _))
        (values `(setf ,place1 ,value1) type)))))

(defun compile-apply (form venv tenv aenv fenv)
  (let ((operator (apply-operator form)))
    (if (built-in-exists-p operator)
        (compile-built-in-apply form venv tenv aenv fenv)
        (compile-user-apply form venv tenv aenv fenv))))

(defun compile-built-in-apply (form venv tenv aenv fenv)
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (let ((argc (built-in-argc operator)))
      (unless (= argc (length operands))
        (error "Invalid number of arguments: ~S" (length operands))))
    (let* ((type (query-appenv form aenv))
           (return-type (function-return-type type)))
      (let ((operator1 (built-in-operator operator type))
            (operands1 (loop for operand in operands
                          collect (compile-form operand venv tenv aenv fenv)))
            (return-type1 (compile-type return-type)))
        (values `(the ,return-type1 (,operator1 ,@operands1)) return-type)))))

(defun compile-user-apply (form venv tenv aenv fenv)
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (let ((argc (funenv-argc operator fenv)))
      (unless (= argc (length operands))
        (error "Invalid number of arguments: ~S" (length operands))))
    (let ((args (funenv-arguments operator fenv)))
      (let ((form1 (%compile-user-apply operator operands args args
                                        venv tenv aenv fenv))
            (return-type (funenv-return-type operator fenv)))
        (values form1 return-type)))))

(defun %compile-user-apply (operator operands args0 args venv tenv aenv fenv)
  (if operands
      (destructuring-bind (operand . operands1) operands
        (destructuring-bind (arg . args1) args
          (multiple-value-bind (operand1 type)
              (compile-form operand venv tenv aenv fenv)
            (multiple-value-bind (venv1 arg1) (extend-varenv arg type venv)
              (cond
                ((or (scalar-type-p type)
                     (array-type-p type))
                 `(let ((,@arg1 ,operand1))
                    ,(%compile-user-apply operator operands1 args0 args1
                                          venv1 tenv aenv fenv)))
                ((vector-type-p type)
                  `(multiple-value-bind ,arg1 ,operand1
                     ,(%compile-user-apply operator operands1 args0 args1
                                           venv1 tenv aenv fenv)))
                 (t (error "Must not be reached.")))))))
      (let ((operator1 (funenv-name1 operator fenv))
            (vars (loop for arg0 in args0
                     append (query-varenv arg0 venv))))
        `(,operator1 ,@vars))))
