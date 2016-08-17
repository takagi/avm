#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.compiler.lisp.compile
  (:use :cl
        :avm
        :avm.lang.data
        :avm.lang.type
        :avm.lang.syntax
        :avm.lang.built-in
        :avm.lang.typenv
        :avm.lang.appenv
        :avm.lang.funenv
        :avm.lang.compiler.lisp.compile-type
        :avm.lang.compiler.lisp.varenv)
  (:export :compile-function
           :*genname-counter*))
(in-package :avm.lang.compiler.lisp.compile)


;;
;; Genname

(defvar *genname-counter* 0)

(defun genname (name)
  (prog1 (intern (format nil "~A~A" name *genname-counter*))
    (incf *genname-counter*)))


;;
;; Compile

(defun compile-function (name ftype args body venv aenv fenv &key entry-p rec-p)
  (assert (not (and entry-p (not rec-p))))
  (let* ((name1 (compile-name name entry-p))
         (pairs (loop for arg in args
                      for type in (function-arg-types ftype)
                   collect (cons arg type)))
         (venv1 (flet ((aux (venv pair)
                         (destructuring-bind (arg . type) pair
                           (extend-varenv arg type venv))))
                  (reduce #'aux pairs :initial-value venv)))
         (fenv1 (if rec-p
                    (extend-funenv-function name name1 ftype args fenv)
                    fenv)))
    (let ((args1 (compile-arguments args venv1))
          (type-decls (compile-type-declarations ftype args venv1))
          (body1 (compile-form body venv1 aenv fenv1)))
      (values name1 args1 `(,@type-decls ,body1)))))

(defun compile-name (name entry-p)
  (if entry-p
      (let ((symbol-name (symbol-name name))
            (symbol-package (symbol-package name)))
        (intern (format nil "%LISP-~A" symbol-name) symbol-package))
      (genname name)))

(defun compile-arguments (args venv)
  (loop for arg in args
     append (query-varenv arg venv)))

(defun compile-type-declarations (ftype args venv)
  `((declare (optimize (speed 3) (safety 0)))
    (declare (ignorable ,@(loop for arg in args
                             append (query-varenv arg venv))))
    ,@(loop for arg in args
            for vars = (query-varenv arg venv)
            for type in (function-arg-types ftype)
         collect
           (cond
             ;; Scalar type and array type.
             ((or (scalar-type-p type)
                  (array-type-p type))
              (let ((type1 (compile-type type)))
                `(declare (type ,type1 ,@vars))))
             ;; Vector type.
             ((vector-type-p type)
              (let ((type1 (compile-type (vector-type-base-type type))))
                `(declare (type ,type1 ,@vars))))
             (t (error "Must not be reached."))))))

(defun compile-form (form venv aenv fenv)
  (cond
    ((literal-p form) (compile-literal form venv aenv fenv))
    ((reference-p form) (compile-reference form venv aenv fenv))
    ((accessor-p form) (compile-accessor form venv aenv fenv))
    ((the-p form) (compile-the form venv aenv fenv))
    ((if-p form) (compile-if form venv aenv fenv))
    ((let-p form) (compile-let form venv aenv fenv))
    ((flet-p form) (compile-flet form venv aenv fenv))
    ((labels-p form) (compile-labels form venv aenv fenv))
    ((setf-p form) (compile-setf form venv aenv fenv))
    ((apply-p form) (compile-apply form venv aenv fenv))
    (t (error "The value ~S is an invalid form." form))))

(defun compile-literal (form venv aenv fenv)
  (declare (ignore venv aenv fenv))
  form)

(defun compile-reference (form venv aenv fenv)
  (declare (ignore aenv fenv))
  (multiple-value-bind (vars type) (query-varenv form venv)
    (cond
      ((or (scalar-type-p type)
           (array-type-p type))
       (car vars))
      ((vector-type-p type)
       (let ((vector-values* (vector-type-values* type)))
         `(,vector-values* ,@vars)))
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

(defun compile-accessor (form venv aenv fenv)
  (compile-apply form venv aenv fenv))

(defun compile-the (form venv aenv fenv)
  (let ((value (the-value form)))
    (compile-form value venv aenv fenv)))

(defun compile-if (form venv aenv fenv)
  (let ((test-form (if-test-form form))
        (then-form (if-then-form form))
        (else-form (if-else-form form)))
    (let ((test-form1 (compile-form test-form venv aenv fenv))
          (then-form1 (compile-form then-form venv aenv fenv))
          (else-form1 (compile-form else-form venv aenv fenv)))
      `(if ,test-form1 ,then-form1 ,else-form1))))

(defun compile-let (form venv aenv fenv)
  (let ((bindings (let-bindings form))
        (body (let-body form)))
    (%compile-let bindings body venv aenv fenv venv)))

(defun %compile-let (bindings body venv aenv fenv venv1)
  (if bindings
      (destructuring-bind ((var form) . bindings1) bindings
        (let* ((form1 (compile-form form venv aenv fenv))
               (type (query-appenv (car bindings) aenv))
               (venv2 (extend-varenv var type venv1))
               (vars (query-varenv var venv2)))
          (cond
            ((or (scalar-type-p type)
                 (array-type-p type))
             (let ((type1 (compile-type type)))
               `(let ((,@vars ,form1))
                  (declare (ignorable ,@vars))
                  (declare (type ,type1 ,@vars))
                  ,(%compile-let bindings1 body venv aenv fenv venv2))))
            ((vector-type-p type)
             (let ((type1 (compile-type (vector-type-base-type type))))
               `(multiple-value-bind ,vars ,form1
                  (declare (ignorable ,@vars))
                  (declare (type ,type1 ,@vars))
                  ,(%compile-let bindings1 body venv aenv fenv venv2))))
            (t (error "Must not be reached.")))))
      (compile-form body venv1 aenv fenv)))

(defun compile-flet (form venv aenv fenv)
  (let ((bindings (flet-bindings form))
        (body (flet-body form)))
    (%compile-flet 'flet bindings body nil venv aenv fenv nil fenv)))

(defun %compile-flet (op bindings body rec-p venv aenv fenv bindings1 fenv1)
  (if bindings
      (destructuring-bind ((name args form) . rest) bindings
        (let ((ftype (query-appenv (car bindings) aenv)))
          (multiple-value-bind (name1 args1 form1)
              (compile-function name ftype args form venv aenv fenv
                                :entry-p nil :rec-p rec-p)
            (let ((bindings2 (cons `(,name1 ,args1 ,@form1) bindings1))
                  (fenv2 (extend-funenv-function name name1 ftype args fenv1)))
              (%compile-flet op rest body rec-p venv aenv fenv
                             bindings2 fenv2)))))
      `(,op ,bindings1
         ,(compile-form body venv aenv fenv1))))

(defun compile-labels (form venv aenv fenv)
  (let ((bindings (labels-bindings form))
        (body (labels-body form)))
    (%compile-flet 'labels bindings body t venv aenv fenv nil fenv)))

(defun compile-setf (form venv aenv fenv)
  (let ((place (setf-place form))
        (value (setf-value form)))
    (let ((place1 (compile-place place venv aenv fenv))
          (value1 (compile-form value venv aenv fenv)))
      `(setf ,place1 ,value1))))

(defun compile-place (place venv aenv fenv)
  ;; Strip type specifier from compiled form to be well optimized.
  (if (reference-place-p place)
      (compile-form place venv aenv fenv)
      (third
       (compile-form place venv aenv fenv))))

(defun compile-apply (form venv aenv fenv)
  (let ((operator (apply-operator form)))
    (if (built-in-exists-p operator)
        (compile-built-in-apply form venv aenv fenv)
        (compile-user-apply form venv aenv fenv))))

(defun compile-built-in-apply (form venv aenv fenv)
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (let ((argc (built-in-argc operator)))
      (unless (= argc (length operands))
        (error "Invalid number of arguments: ~S" (length operands))))
    (let* ((type (query-appenv form aenv))
           (return-type (function-return-type type)))
      (let ((operator1 (built-in-operator :lisp operator type))
            (operands1 (loop for operand in operands
                          collect (compile-form operand venv aenv fenv)))
            (return-type1 (compile-type return-type)))
        `(the ,return-type1 (,operator1 ,@operands1))))))

(defun compile-user-apply (form venv aenv fenv)
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (let ((argc (funenv-function-argc operator fenv)))
      (unless (= argc (length operands))
        (error "Invalid number of arguments: ~S" (length operands))))
    (let ((args (funenv-function-arguments operator fenv))
          (arg-types (funenv-function-arg-types operator fenv)))
      (%compile-user-apply operator operands args arg-types
                           venv aenv fenv nil))))

(defun %compile-user-apply (operator operands args arg-types venv aenv fenv
                            vars1)
  (if operands
      (destructuring-bind (operand . operands1) operands
        (destructuring-bind (arg . args1) args
          (destructuring-bind (type . arg-types1) arg-types
            (let* ((operand1 (compile-form operand venv aenv fenv))
                   (venv1 (extend-varenv arg type venv))
                   (vars (query-varenv arg venv1))
                   (vars2 (append vars vars1)))
              (cond
                ((or (scalar-type-p type)
                     (array-type-p type))
                 (let ((type1 (compile-type type)))
                   `(let ((,@vars ,operand1))
                      (declare (type ,type1 ,@vars))
                      ,(%compile-user-apply operator operands1 args1 arg-types1
                                            venv1 aenv fenv vars2))))
                ((vector-type-p type)
                 (let ((type1 (compile-type (vector-type-base-type type))))
                   `(multiple-value-bind ,vars ,operand1
                      (declare (type ,type1 ,@vars))
                      ,(%compile-user-apply operator operands1 args1 arg-types1
                                            venv1 aenv fenv vars2))))
                (t (error "Must not be reached.")))))))
      (let* ((operator1 (funenv-function-name1 operator fenv))
             (return-type (funenv-function-return-type operator fenv))
             (return-type1 (compile-type return-type)))
        `(the ,return-type1 (,operator1 ,@(nreverse vars1))))))
