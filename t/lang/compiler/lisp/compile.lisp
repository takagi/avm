#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm-test.lang.compiler.lisp.compile
  (:use :cl
        :prove
        :avm
        :avm.lang.typenv
        :avm.lang.appenv
        :avm.lang.funenv
        :avm.lang.compiler.lisp.varenv
        :avm.lang.compiler.lisp.compile))
(in-package :avm-test.lang.compiler.lisp.compile)


(plan nil)


(defmacro with-env ((tenv aenv fenv venv) &body body)
  `(let ((,tenv (empty-typenv))
         (,aenv (empty-appenv))
         (,fenv (empty-funenv))
         (,venv (empty-varenv))
         (*genvar-counter* 0)
         (*genname-counter* 0))
     ,@body))


;;
;; COMPILE-FUNCTION

(subtest "compile-function"

  (with-env (tenv aenv fenv venv)
    (let ((aenv1 (extend-appenv '#1=(+ x y)
                                '((:vector int 3) (:vector int 3)
                                  (:vector int 3))
                                aenv)))
      (is-values (compile-function 'foo
                                   '((:vector int 3) (:vector int 3) int)
                                   '(x y)
                                   '#1#    ; (+ x y)
                                   venv tenv aenv1 fenv)
                 '(foo0 (x0 x1 x2 y3 y4 y5)
                   ((declare (optimize (speed 3) (safety 0)))
                    (declare (ignorable x0 x1 x2))
                    (declare (ignorable y3 y4 y5))
                    (declare (type fixnum x0 x1 x2))
                    (declare (type fixnum y3 y4 y5))
                    (the (values fixnum fixnum fixnum)
                     (avm.lang.data:int3-add*
                      (avm.lang.data:int3-values* x0 x1 x2)
                      (avm.lang.data:int3-values* y3 y4 y5)))))
                 "Base case - vector type arguments."))))


;;
;; COMPILE-PROGN

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'compile-progn)
        #'avm.lang.compiler.lisp.compile::compile-progn))

(subtest "compile-progn"

  (with-env (tenv anev fenv venv)
    (is (compile-progn '(progn 1 2 3) venv tenv aenv fenv)
        `(progn 1 2 3)
        "Base case."))

  (with-env (tenv anev fenv venv)
    (is-error (compile-progn '(progn) venv tenv aenv fenv)
              simple-error
              "Invalid form.")))


;;
;; COMPILE-LET

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'compile-let)
        #'avm.lang.compiler.lisp.compile::compile-let))

(subtest "compile-let"

  (with-env (tenv aenv fenv venv)
    (is (compile-let '(let ((x 1)) x) venv tenv aenv fenv)
        '(let ((x0 1))
           (declare (type fixnum x0))
           x0)
        "Base type - scalar type."))

  (with-env (tenv aenv fenv venv)
    (let ((aenv1 (extend-appenv '#1=(int2 1 1)
                                '(int int (:vector int 2))
                                aenv)))
      (is (compile-let '(let ((x #1#)) x) venv tenv aenv1 fenv)
          '(multiple-value-bind (x0 x1)
              (the (values fixnum fixnum) (avm.lang.data::int2-values* 1 1))
             (declare (type fixnum x0))
             (declare (type fixnum x1))
             (avm.lang.data::int2-values* x0 x1))
          "Base type - vector type.")))

  (with-env (tenv aenv fenv venv)
    (let ((tenv1 (extend-typenv 'as '(:array int) tenv))
          (venv1 (extend-varenv 'as '(:array int) venv)))
      (is (compile-let '(let ((x as)) x) venv1 tenv1 aenv fenv)
          '(let ((x1 as0))
             (declare (type int-array x1))
             x1)
          "Base type - array type.")))

  (with-env (tenv aenv fenv venv)
    (let ((aenv1 (extend-appenv '#2=(int2 1 1)
                                '(int int (:vector int 2))
                                aenv)))
      (is (compile-let '(let ((x 1) (y #2#)) x) venv tenv aenv1 fenv)
          '(let ((x0 1))
             (declare (type fixnum x0))
             (multiple-value-bind (y1 y2)
                 (the (values fixnum fixnum) (avm.lang.data::int2-values* 1 1))
               (declare (type fixnum y1))
               (declare (type fixnum y2))
               x0))
          "Base type - multiple bindings.")))

  (with-env (tenv aenv fenv venv)
    (is (compile-let '(let ((x 1)) x x) venv tenv aenv fenv)
        '(let ((x0 1))
           (declare (type fixnum x0))
           x0
           x0)
        "Base type - implicit progn.")))


;;
;; COMPILE-BUILT-IN-APPLY

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'compile-built-in-apply)
        #'avm.lang.compiler.lisp.compile::compile-built-in-apply))

(subtest "compile-built-in-apply"

  (with-env (tenv aenv fenv venv)
    (let ((aenv1 (extend-appenv '#1=(+ 1 1) '(int int int) aenv)))
      (is (compile-built-in-apply '#1# venv tenv aenv1 fenv)
          '(the fixnum (+ 1 1))
          "Base case - int addition.")))

  (with-env (tenv aenv fenv venv)
    (let ((aenv1 (extend-appenv '#2=(coerce 1) '(int double) aenv)))
      (is (compile-built-in-apply '#2# venv tenv aenv1 fenv)
          '(the double-float (avm.lang.compiler.lisp.built-in::int->double 1))
          "Base case - COERCE of int to double.")))

  (with-env (tenv aenv fenv venv)
    (is-error (compile-built-in-apply '(+ 1 1) venv tenv aenv fenv)
              simple-error
              "Not exist in appenv."))

  (with-env (tenv aenv fenv venv)
    (let ((aenv1 (extend-appenv '#3=(+ 1 1 1) '(int int int) aenv)))
      (is-error (compile-built-in-apply '#3# venv tenv aenv1 fenv)
                simple-error
                "Invalid number of arguments."))))


(finalize)
