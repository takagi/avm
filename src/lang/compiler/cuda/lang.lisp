#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.compiler.cuda.lang
  (:use :cl
        :avm
        :avm.lang
        :avm.lang.kernel
        :avm.lang.expand-macro
        :avm.lang.convert-implicit-progn
        :avm.lang.binarize
        :avm.lang.convert-functions
        :avm.lang.free-variable
        :avm.lang.typenv
        :avm.lang.unienv
        :avm.lang.funenv
        :avm.lang.appenv
        :avm.lang.infer
        :avm.lang.compiler.cuda.k-normal
        :avm.lang.compiler.cuda.compile
        )
  (:shadowing-import-from :avm.lang.expand-macro
                          :expand-macro))
(in-package :avm.lang.compiler.cuda.lang)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'kernel->vars)
        #'avm.lang.compiler.lisp.lang::kernel->vars)

  (setf (fdefinition 'kernel->typenv)
        #'avm.lang.compiler.lisp.lang::kernel->typenv)

  (setf (fdefinition 'subst-ftype)
        #'avm.lang.compiler.lisp.lang::subst-ftype))

(defun %extend-functions (kernel funenv)
  (flet ((aux (funenv1 name)
           (let ((cuda-name (kernel-function-cuda-name kernel name))
                 (type (kernel-function-type kernel name))
                 (args (kernel-function-arguments kernel name)))
             (extend-funenv name cuda-name type args funenv1))))
    (reduce #'aux (kernel-function-names kernel)
            :initial-value funenv)))

(defun kernel->funenv (kernel)
   (%extend-functions kernel
    (empty-funenv)))

(defmethod compile-kernel-function ((engine (eql :cuda)) name args body kernel)
  (let* ((fenv (kernel->funenv kernel))
         (body1 (convert-functions
                 (binarize
                  (convert-implicit-progn
                   (expand-macro body fenv))))))
    ;; Check free variable existence.
    (let ((vars (kernel->vars kernel)))
      (check-free-variable args body1 vars))
    ;; K-normalization.
    (let ((body2 (k-normal body1)))
      ;; Type inference.
      (let ((tenv (kernel->typenv kernel))
            (aenv (empty-appenv))
            (uenv (empty-unienv)))
        (multiple-value-bind (ftype aenv1 uenv1)
            (infer-function name args body2 tenv aenv uenv fenv)
          ;; Compilation.
          (let ((aenv2 (subst-appenv uenv1 aenv1))
                (ftype1 (subst-ftype uenv1 ftype)))
            (multiple-value-bind (caller-name cuda-name defkernels)
                (compile-function name ftype1 args body2 aenv2 fenv nil
                                  :entry-p t :rec-p t)
              (values caller-name cuda-name ftype1
                      `(progn ,@(nreverse defkernels))))))))))

(defmethod compile-kernel-global (kernel name (engine (eql :cuda)))
  nil)

(defmethod compile-kernel-constant (kernel name (engine (eql :cuda)))
  nil)
