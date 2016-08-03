#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.compiler.cuda.compile
  (:use :cl
        :avm
        :avm.lang.type
        :avm.lang.syntax
        :avm.lang.built-in
        :avm.lang.appenv
        :avm.lang.funenv
        :avm.lang.compiler.cuda.compile-type
        )
  (:export :compile-function
           :*genname-counter*))
(in-package :avm.lang.compiler.cuda.compile)


;;
;; Genname

(defvar *genname-counter* 0)

(defun genname (name)
  (prog1 (intern (format nil "%~A~A" name *genname-counter*))
    (incf *genname-counter*)))


;;
;; Compile

(defun compile-function (name ftype args body aenv fenv funcs
                         &key entry-p rec-p)
  (assert (not (and entry-p (not rec-p))))
  (let* ((cuda-name (compile-name name entry-p nil))
         (fenv1 (if rec-p
                    (extend-funenv name cuda-name ftype args fenv)
                    fenv)))
    (multiple-value-bind (body1 funcs1)
        (compile-form body :tail aenv fenv1 funcs)
      (let ((func (%compile-function cuda-name ftype args body1)))
        (if (not entry-p)
            (values cuda-name (cons func funcs1))
            (multiple-value-bind (caller-name caller-func)
                (%compile-caller-function name cuda-name ftype args)
              (values caller-name
                      cuda-name
                      (cons caller-func
                            (cons func funcs1)))))))))

(defun %compile-function (cuda-name ftype args body)
  (let ((args1 (loop for arg in args
                     for type in (function-arg-types ftype)
                     for type1 = (compile-type type)
                  collect (list arg type1)))
        (return-type (compile-type
                      (function-return-type ftype))))
    `(cl-cuda:defkernel ,cuda-name (,return-type ,args1)
       ,body)))

(defun %compile-caller-function (name cuda-name ftype args)
  (let ((caller-name (compile-name name t t))
        (args1 (loop for arg in args
                     for type in (function-arg-types ftype)
                     for type1 = (compile-type type)
                  collect (list arg type1))))
    (let ((func `(cl-cuda:defkernel ,caller-name (cl-cuda:void ,(cdr args1))
                   (let ((i (+ (* cl-cuda:block-dim-x cl-cuda:block-idx-x)
                               cl-cuda:thread-idx-x)))
                     (when (< i n)
                       (,cuda-name ,@args))))))
      (values caller-name func))))

(defun compile-name (name entry-p caller-p)
  (assert (not (and (not entry-p) caller-p)))
  (if entry-p
      (let ((symbol-name (symbol-name name))
            (symbol-package (symbol-package name)))
        (intern (format nil "%~@[CALL-~*~]CUDA-~A" caller-p symbol-name)
                symbol-package))
      (genname name)))

(defun compile-form (form dest aenv fenv funcs)
  (cond
    ((literal-p form) (compile-literal form dest aenv fenv funcs))
    ((reference-p form) (compile-reference form dest aenv fenv funcs))
    ((accessor-p form) (compile-accessor form dest aenv fenv funcs))
    ((the-p form) (compile-the form dest aenv fenv funcs))
    ((if-p form) (compile-if form dest aenv fenv funcs))
    ((let-p form) (compile-let form dest aenv fenv funcs))
    ((flet-p form) (compile-flet form dest aenv fenv funcs))
    ((labels-p form) (compile-labels form dest aenv fenv funcs))
    ((setf-p form) (compile-setf form dest aenv fenv funcs))
    ((apply-p form) (compile-apply form dest aenv fenv funcs))
    (t (error "The value ~S is an invalid form." form))))

(defun compile-literal (form dest aenv fenv funcs)
  (declare (ignore aenv fenv))
  (cl-pattern:match dest
    (:tail (values `(return ,form) funcs))
    ((:non-tail var) (values `(set ,var ,form) funcs))
    (_ (error "The value ~S is an invalid destination." dest))))

(defun compile-reference (form dest aenv fenv funcs)
  (declare (ignore aenv fenv))
  (cl-pattern:match dest
    (:tail (values `(return ,form) funcs))
    ((:non-tail var) (values `(set ,var ,form) funcs))
    (_ (error "The value ~S is an invalid destination." dest))))

(defun compile-accessor (form dest aenv fenv funcs)
  (compile-apply form dest aenv fenv funcs))

(defun compile-the (form dest aenv fenv funcs)
  (let ((value (the-value form)))
    (compile-form value dest aenv fenv funcs)))

(defun compile-if (form dest aenv fenv funcs)
  ;; Assuming K-normalized.
  (let ((test-form (if-test-form form))
        (then-form (if-then-form form))
        (else-form (if-else-form form)))
    (multiple-value-bind (then-form1 funcs1)
        (compile-form then-form dest aenv fenv funcs)
      (multiple-value-bind (else-form1 funcs2)
          (compile-form else-form dest aenv fenv funcs1)
        (values `(if ,test-form ,then-form1 ,else-form1) funcs2)))))

(defun compile-let (form dest aenv fenv funcs)
  (let ((bindings (let-bindings form))
        (body (let-body form)))
    (%compile-let bindings body dest aenv fenv funcs)))

(defun type-zero (type)
  (cl-pattern:match type
    ('bool nil)
    ('int 0)
    ('float 0.0)
    ('double 0.0d0)
    ;; ((:vector 'int 2) '(cl-cuda:int2 0 0))
    ;; ((:vector 'int 3) '(cl-cuda:int3 0 0 0))
    ;; ((:vector 'int 4) '(cl-cuda:int4 0 0 0 0))
    ;; ((:vector 'float 2) '(cl-cuda:float2 0.0 0.0))
    ((:vector 'float 3) '(cl-cuda:float3 0.0 0.0 0.0))
    ((:vector 'float 4) '(cl-cuda:float4 0.0 0.0 0.0 0.0))
    ;; ((:vector 'double 2) '(cl-cuda:double2 0.0d0 0.0d0))
    ((:vector 'double 3) '(cl-cuda:double3 0.0d0 0.0d0 0.0d0))
    ((:vector 'double 4) '(cl-cuda:double4 0.0d0 0.0d0 0.0d0 0.0d0))
    (_ (error "The value ~S is an invalid type." type))))

(defun %compile-let (bindings body dest aenv fenv funcs)
  (if bindings
      (destructuring-bind ((var value) . bindings1) bindings
        (multiple-value-bind (value1 funcs1)
            (compile-form value `(:non-tail ,var) aenv fenv funcs)
          (multiple-value-bind (body1 funcs2)
              (%compile-let bindings1 body dest aenv fenv funcs1)
            (let ((type (query-appenv (car bindings) aenv)))
              (values `(let ((,var ,(type-zero type)))
                         ,value1
                         ,body1)
                      funcs2)))))
      (compile-form body dest aenv fenv funcs)))

(defun compile-flet (form dest aenv fenv funcs)
  (let ((bindings (flet-bindings form))
        (body (flet-body form)))
    (%compile-flet bindings body dest nil aenv fenv fenv funcs)))

(defun %compile-flet (bindings body dest rec-p aenv fenv fenv1 funcs)
  (if bindings
      (destructuring-bind ((name args form) . bindings1) bindings
        (let ((ftype (query-appenv (car bindings) aenv)))
          (multiple-value-bind (name1 funcs1)
              (compile-function name ftype args form aenv fenv funcs
                                :entry-p nil :rec-p rec-p)
            (let ((fenv2 (extend-funenv-function name name1 ftype args fenv1)))
              (%compile-flet bindings1 body dest rec-p
                             aenv fenv fenv2 funcs1)))))
      (compile-form body dest aenv fenv1 funcs)))

(defun compile-labels (form dest aenv fenv funcs)
  (let ((bindings (labels-bindings form))
        (body (labels-body form)))
    (%compile-flet bindings body dest t aenv fenv fenv funcs)))

(defun compile-setf (form dest aenv fenv funcs)
  ;; Assuming K-normalized.
  (let ((place (setf-place form))
        (value (setf-value form)))
      (multiple-value-bind (value1 funcs1)
          (compile-form value dest aenv fenv funcs)
        (values `(progn
                   (set ,place ,value)
                   ,value1)
                funcs1))))

(defun compile-apply (form dest aenv fenv funcs)
  ;; Assuming K-normalized.
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (let ((argc (if (built-in-exists-p operator)
                    (built-in-argc operator)
                    (funenv-function-argc operator fenv))))
      (unless (= argc (length operands))
        (error "Invalid number of arguments: ~S" (length operands))))
    (let ((operator1 (if (built-in-exists-p operator)
                         (built-in-operator :cuda operator
                          (query-appenv form aenv))
                         (funenv-function-name1 operator fenv))))
      (cl-pattern:match dest
        (:tail
         (values `(return (,operator1 ,@operands)) funcs))
        ((:non-tail var)
         (values `(set ,var (,operator1 ,@operands)) funcs))
        (_ (error "The value ~S is an invalid destination." dest))))))
