#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo-test.lang.compiler.cl.compile-form
  (:use :cl
        :prove
        :foo
        :foo.lang.typenv
        :foo.lang.appenv
        :foo.lang.funenv
        :foo.lang.compiler.cl.varenv
        :foo.lang.compiler.cl.compile-form))
(in-package :foo-test.lang.compiler.cl.compile-form)


(plan nil)


(defmacro with-env ((tenv aenv fenv venv) &body body)
  `(let ((,tenv (empty-typenv))
         (,aenv (empty-appenv))
         (,fenv (empty-funenv))
         (,venv (empty-varenv))
         (foo.lang.compiler.cl.varenv::*genvar-counter* 0))
     ,@body))


(subtest "LET"

  (with-env (tenv aenv fenv venv)
    (is (compile-form '(let ((x 1)) x) venv tenv aenv fenv)
        '(let ((x0 1))
           (declare (type fixnum x0))
           x0)
        "Ok - Scalar type."))

  (with-env (tenv aenv fenv venv)
    (let ((aenv1
           (extend-appenv '#1=(int2 1 1) '(int int (:vector int 2)) aenv)))
      (is (compile-form '(let ((x #1#)) x) venv tenv aenv1 fenv)
          '(multiple-value-bind (x0 x1)
               (the (values fixnum fixnum) (foo.lang.data::int2-values* 1 1))
             (declare (type fixnum x0))
             (declare (type fixnum x1))
             (foo.lang.data::int2-values* x0 x1))
          "Ok - Vector type.")))

  (with-env (tenv aenv fenv venv)
    (let ((tenv1 (extend-typenv 'as '(:array int) tenv))
          (venv1 (extend-varenv 'as '(:array int) venv)))
      (is (compile-form '(let ((x as)) x) venv1 tenv1 aenv fenv)
          '(let ((x1 as0))
             (declare (type int-array x1))
             x1)
          "Ok - Array type.")))

  (with-env (tenv aenv fenv venv)
    (let ((aenv1
           (extend-appenv '#2=(int2 1 1) '(int int (:vector int 2)) aenv)))
      (is (compile-form '(let ((x 1) (y #2#)) x) venv tenv aenv1 fenv)
          '(let ((x0 1))
             (declare (type fixnum x0))
             (multiple-value-bind (y1 y2)
                 (the (values fixnum fixnum) (foo.lang.data::int2-values* 1 1))
               (declare (type fixnum y1))
               (declare (type fixnum y2))
               x0))
          "Ok - Multiple bindings.")))
  )

(subtest "Built-in apply"

  (with-env (tenv aenv fenv venv)
    (let ((aenv1 (extend-appenv '#1=(coerce 1) '(int double) aenv)))
      (is (compile-form '#1# venv tenv aenv1 fenv)
          '(the double-float (foo.lang.compiler.cl.built-in::int->double 1))
          "Ok.")))

  (with-env (tenv aenv fenv venv)
    (is-error (compile-form '(coerce 1) venv tenv aenv fenv)
              simple-error
              "Not exist in appenv."))

  (with-env (tenv aenv fenv venv)
    (let ((aenv1 (extend-appenv '#2=(+ 1 1 1) '(int int int) aenv)))
      (is-error (compile-form '#2# venv tenv aenv1 fenv)
                simple-error
                "Invalid number of arguments.")))
  )


(finalize)
