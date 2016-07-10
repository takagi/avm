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
        :foo.lang.funenv
        :foo.lang.compiler.cl.varenv
        :foo.lang.compiler.cl.compile-form))
(in-package :foo-test.lang.compiler.cl.compile-form)

(plan nil)

(subtest "LET"

  (macrolet ((with-env ((tenv fenv venv) &body body)
               `(let ((,tenv (empty-typenv))
                      (,fenv (empty-funenv))
                      (,venv (empty-varenv))
                      (foo.lang.compiler.cl.varenv::*genvar-counter* 0))
                  ,@body)))

    (with-env (tenv fenv venv)
      (prove:is (compile-form '(let ((x 1))
                                 x)
                              venv tenv fenv)
                '(let ((x0 1))
                   (declare (type fixnum x0))
                   x0)
                "Ok. - scalar type"))

    (with-env (tenv fenv venv)
      (prove:is (compile-form '(let ((x (int2 1 1)))
                                 x)
                              venv tenv fenv)
                '(multiple-value-bind (x0 x1) (foo.lang.data::int2-values* 1 1)
                   (declare (type fixnum x0))
                   (declare (type fixnum x1))
                   (foo.lang.data::int2-values* x0 x1))
                "Ok. - vector type"))

    (with-env (tenv fenv venv)
      (let ((tenv1 (extend-typenv 'as '(:array int) tenv))
            (venv1 (extend-varenv 'as '(:array int) venv)))
        (prove:is (compile-form '(let ((x as))
                                   x)
                                venv1 tenv1 fenv)
                  '(let ((x1 as0))
                     (declare (type int-array x1))
                     x1)
                  "Ok. - array type")))

    (with-env (tenv fenv venv)
      (prove:is (compile-form '(let ((x 1)
                                     (y (int2 1 1)))
                                 x)
                              venv tenv fenv)
                '(let ((x0 1))
                   (declare (type fixnum x0))
                   (multiple-value-bind (y1 y2)
                       (foo.lang.data::int2-values* 1 1)
                     (declare (type fixnum y1))
                     (declare (type fixnum y2))
                     x0))
                "Ok. - multiple bindings"))
    ))

(finalize)

