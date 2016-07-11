#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo.lang.compiler.cl.lang
  (:use :cl
        :foo
        :foo.lang
        :foo.lang.type
        :foo.lang.kernel
        :foo.lang.binarize
        :foo.lang.convert-functions
        :foo.lang.typenv
        :foo.lang.unienv
        :foo.lang.funenv
        :foo.lang.infer
        :foo.lang.compiler.cl.compile-type
        :foo.lang.compiler.cl.varenv
        :foo.lang.compiler.cl.compile-form
        ))
(in-package :foo.lang.compiler.cl.lang)


(defun %extend-arguments-typenv (args typenv)
  (flet ((aux (typenv1 arg)
           (let ((type (if (member arg '(i n))
                           'int
                           (gentype))))
             (extend-typenv arg type typenv1))))
    (reduce #'aux args :initial-value typenv)))

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

(defun kernel->typenv (kernel args)
  (declare (ignore kernel))
  (%extend-arguments-typenv args
;   (%extend-constants-typenv kernel
;    (%extend-globals-typenv kernel
     (empty-typenv)));))

(defun %extend-self (name args typenv funenv)
  (let ((name1 (convert-function-name name :cl))
        (type (append
               (loop for arg in args
                  collect (query-typenv arg typenv))
               (list (gentype)))))
    (extend-funenv name1 type args funenv)))

(defun %extend-functions (kernel funenv)
  (flet ((aux (funenv1 name)
           (let ((type (kernel-function-type kernel name))
                 (args (kernel-function-arguments kernel name)))
             (extend-funenv name type args funenv1))))
    (reduce #'aux (kernel-function-names kernel)
            :initial-value funenv)))

(defun kernel->funenv (kernel name args typenv)
  (%extend-self name args typenv
   (%extend-functions kernel
    (empty-funenv))))

(defun %extend-arguments-varenv (args typenv varenv)
  (flet ((aux (varenv1 arg)
           (let ((type (query-typenv arg typenv)))
             (extend-varenv arg type varenv1))))
    (reduce #'aux args :initial-value varenv)))

(defun kernel->varenv (args tenv1)
  (%extend-arguments-varenv args tenv1
;   (%extend-constants-varenv kernel tenv1
;    (%extend-globals-varenv kernel tenv1
     (empty-varenv)));))

(defmethod compile-kernel-function ((engine (eql :cl)) name args body kernel)
  (let ((body1 (convert-functions :cl
                (binarize body))))
    ;; Type inference.
    (let* ((tenv (kernel->typenv kernel args))
           (uenv (empty-unienv))
           (fenv (kernel->funenv kernel name args tenv)))
        (multiple-value-bind (return-type uenv1) (infer body1 tenv uenv fenv)
          ;; Compilation.
          (let* ((tenv1 (subst-typenv uenv1 tenv))
                 (venv (kernel->varenv args tenv1)))
            (let ((name1 (convert-function-name name :cl))
                  (type (append
                         (loop for arg in args
                            collect (query-typenv arg tenv1))
                         (list return-type)))
                  (args1 (loop for arg in args
                            append (query-varenv arg venv)))
                  (body2 (compile-form body1 venv tenv1 fenv)))
              (values name1 type
                      `(defun ,name1 ,args1
                         (declare (optimize (speed 3) (safety 0)))
                         (declare (ignorable ,@args1))
                         ,@(loop for arg1 in args1
                                 for arg-type in type
                              collect
                                (let ((arg-type1 (compile-type arg-type)))
                                  `(declare (type ,arg-type1 ,arg1))))
                         ,body2))))))))


(defmethod compile-kernel-global (kernel name (engine (eql :cl)))
  nil)

(defmethod compile-kernel-constant (kernel name (engine (eql :cl)))
  nil)
