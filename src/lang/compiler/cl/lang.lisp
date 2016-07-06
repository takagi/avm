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
        :foo.lang.typenv
        :foo.lang.unienv
        :foo.lang.funenv
        :foo.lang.infer
        :foo.lang.compiler.cl.varenv
        :foo.lang.compiler.cl.compile-form
        ))
(in-package :foo.lang.compiler.cl.lang)


(defun %extend-arguments-typenv (args typenv)
  (flet ((aux (typenv1 arg)
           (let ((type (gentype)))
             (extend-typenv arg type typenv1))))
    (reduce #'aux args :initial-value typenv)))

;; (defun %extend-constants (kernel typenv)
;;   (flet ((aux (typenv1 name)
;;            (let ((type (kernel-constant-type kernel name)))
;;              (extend-typenv name type typenv1))))
;;     (reduce #'aux (kernel-constant-names kernel) :initial-value typenv)))

;; (defun %extend-globals (kernel typenv)
;;   (flet ((aux (typenv1 name)
;;            (let ((type (kernel-global-type kernel name)))
;;              (extend-typenv name type typenv1))))
;;     (reduce #'aux (kernel-global-names kernel) :initial-value typenv)))

(defun kernel->typenv (kernel args)
  (%extend-arguments-typenv args
;   (%extend-constants-typenv kernel
;    (%extend-globals-typenv kernel
     (empty-typenv)));))

;; (defun %extend-functions (kernel funenv)
;;   (flet ((aux (funenv1 name)
;;            (let ((type (kernel-function-type kernel name)))
;;              (extend-funenv name type funenv1))))
;;     (reduce #'aux (kernel-function-names kernel)
;;             :initial-value funenv)))

(defun kernel->funenv (kernel)
  (declare (ignore kernel))
;  (%extend-functions kernel
   (empty-funenv));)

(defmethod compile-kernel-function ((engine (eql :cl)) name args body kernel)
  (let ((name1 (foo.lang.compiler.cl.compile-form::compile-function-name name))
        (body1 (binarize body)))
    (let ((tenv (kernel->typenv kernel args))
          (uenv (empty-unienv))
          (fenv (kernel->funenv kernel)))
        (multiple-value-bind (return-type uenv1) (infer body1 tenv uenv fenv)
          (let ((venv (empty-varenv))
                (tenv1 (subst-typenv uenv1 tenv)))
            (let ((body2 (compile-form body1 venv tenv1 fenv))
                  (type (append
                         (loop for arg in args
                            collect (query-typenv arg tenv1))
                         (list return-type))))
              (values name1 type body2)))))))

(defmethod compile-kernel-global (kernel name (engine (eql :cl)))
  nil)

(defmethod compile-kernel-constant (kernel name (engine (eql :cl)))
  nil)
