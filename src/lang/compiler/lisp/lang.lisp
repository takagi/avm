#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.compiler.lisp.lang
  (:use :cl
        :avm
        :avm.lang
        :avm.lang.type
        :avm.lang.kernel
        :avm.lang.binarize
        :avm.lang.convert-functions
        :avm.lang.free-variable
        :avm.lang.typenv
        :avm.lang.unienv
        :avm.lang.appenv
        :avm.lang.funenv
        :avm.lang.infer
        :avm.lang.compiler.lisp.compile-type
        :avm.lang.compiler.lisp.varenv
        :avm.lang.compiler.lisp.compile
        ))
(in-package :avm.lang.compiler.lisp.lang)


(defun kernel->vars (kernel)
  (declare (ignore kernel))
;  (%extend-constants-vars kernel
;   (%extend-globals-vars kernel
    nil);))

;; (defun %extend-constants-typenv (kernel typenv)
;;   (flet ((aux (typenv1 name)
;;            (let ((type (kernel-constant-type kernel name)))
;;              (extend-typenv name type typenv1))))
;;     (reduce #'aux (kernel-constant-names kernel) :initial-value typenv)))

;; (defun %extend-globals-typenv (kernel typenv)
;;   (flet ((aux (typenv1 name)
;;            (let ((type (kernel-global-type kernel name)))
;;              (extend-typenv name type typenv1))))
;;     (reduce #'aux (kernel-global-names kernel) :initial-value typenv)))

(defun kernel->typenv (kernel)
  (declare (ignore kernel))
;   (%extend-constants-typenv kernel
;    (%extend-globals-typenv kernel
     (empty-typenv));))

(defun %extend-functions (kernel funenv)
  (flet ((aux (funenv1 name)
           (let ((name1 (kernel-function-cl-name kernel name))
                 (type (kernel-function-type kernel name))
                 (args (kernel-function-arguments kernel name)))
             (extend-funenv name name1 type args funenv1))))
    (reduce #'aux (kernel-function-names kernel)
            :initial-value funenv)))

(defun kernel->funenv (kernel)
   (%extend-functions kernel
    (empty-funenv)))

(defun subst-ftype (uenv ftype)
  (loop for type in ftype
     collect (query-unienv type uenv)))

(defmethod compile-kernel-function ((engine (eql :cl)) name args body kernel)
  (let ((args1 (append '(i n) args))
        (body1 (convert-functions
                (binarize body))))
    ;; Check free variable existence.
    (let ((vars (kernel->vars kernel)))
      (check-free-variable args1 body1 vars))
    ;; Type inference.
    (let* ((tenv (kernel->typenv kernel))
           (aenv (empty-appenv))
           (uenv (empty-unienv))
           (fenv (kernel->funenv kernel)))
      (multiple-value-bind (ftype aenv1 uenv1)
          (infer-function name args1 body1 tenv aenv uenv fenv)
        ;; Compilation.
        (let ((tenv1 (subst-typenv uenv1 tenv))
              (aenv2 (subst-appenv uenv1 aenv1))
              (ftype1 (subst-ftype uenv1 ftype))
              (venv (empty-varenv)))
          (multiple-value-bind (name1 args2 body2)
              (compile-function name ftype1 args1 body1 venv tenv1 aenv2 fenv
                                :entry-p t :rec-p t)
            (values name1 ftype1 args1 `(defun ,name1 ,args2 ,@body2))))))))

(defmethod compile-kernel-global (kernel name (engine (eql :cl)))
  nil)

(defmethod compile-kernel-constant (kernel name (engine (eql :cl)))
  nil)
