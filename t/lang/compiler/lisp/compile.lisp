#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm-test.lang.compiler.lisp.compile
  (:use :cl
        :prove
        :avm
        :avm.lang.data
        :avm.lang.typenv
        :avm.lang.appenv
        :avm.lang.funenv
        :avm.lang.compiler.lisp.varenv
        :avm.lang.compiler.lisp.compile))
(in-package :avm-test.lang.compiler.lisp.compile)


(plan nil)


(defmacro with-env ((aenv fenv venv) &body body)
  `(let ((,aenv (empty-appenv))
         (,fenv (empty-funenv))
         (,venv (empty-varenv))
         (*genvar-counter* 0)
         (*genname-counter* 0))
     ,@body))


;;
;; COMPILE-FUNCTION

(subtest "compile-function"

  (with-env (aenv fenv venv)
    (let ((aenv1 (extend-appenv '#1=(+ x y)
                                '((:vector int 3) (:vector int 3)
                                  (:vector int 3))
                                aenv)))
      (is-values (compile-function 'foo
                                   '((:vector int 3) (:vector int 3) int)
                                   '(x y)
                                   '#1#    ; (+ x y)
                                   venv aenv1 fenv)
                 '(foo0 (x0 x1 x2 y3 y4 y5)
                   ((declare (optimize (speed 3) (safety 0)))
                    (declare (ignorable x0 x1 x2 y3 y4 y5))
                    (declare (type fixnum x0 x1 x2))
                    (declare (type fixnum y3 y4 y5))
                    (the (values fixnum fixnum fixnum)
                     (avm.lang.data:int3-add*
                      (avm.lang.data:int3-values* x0 x1 x2)
                      (avm.lang.data:int3-values* y3 y4 y5)))))
                 "Base case - vector type arguments."))))


;;
;; COMPILE-SETF

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'compile-setf)
        #'avm.lang.compiler.lisp.compile::compile-setf))

(subtest "compile-setf"

  (with-env (aenv fenv venv)
    (let ((venv1 (extend-varenv 'x 'int venv)))
      (is (compile-setf '(setf x 1) venv1 aenv fenv)
          '(setf x0 1)
          "Base case - reference place of int type.")))

  (with-env (aenv fenv venv)
    (let ((form '(setf x #1=(int2 1 1)))
          (venv1 (extend-varenv 'x '(:vector int 2) venv))
          (aenv1 (extend-appenv '#1# '(int int (:vector int 2)) aenv)))
      (is (compile-setf form venv1 aenv1 fenv)
          '(setf (int2-values* x0 x1)
                 (the (values fixnum fixnum) (int2-values* 1 1)))
          "Base case - reference place of int2 type.")))

  (with-env (aenv fenv venv)
    (let ((form '(setf #2=(int2-x x) 1))
          (venv1 (extend-varenv 'x '(:vector int 2) venv))
          (aenv1 (extend-appenv '#2# '((:vector int 2) int) aenv)))
      (is (compile-setf form venv1 aenv1 fenv)
          '(setf (int2-x* (int2-values* x0 x1)) 1)
          "Base case - vector place of int2 type.")))

  (with-env (aenv fenv venv)
    (let ((form '(setf #3=(aref x 0) 1))
          (venv1 (extend-varenv 'x '(:array int) venv))
          (aenv1 (extend-appenv '#3# '((:array int) int int) aenv)))
      (is (compile-setf form venv1 aenv1 fenv)
          '(setf (aref x0 0) 1)
          "Base case - array place of int type.")))

  (with-env (aenv fenv venv)
    (let ((form '(setf #4=(aref x 0) #5=(int2 1 1)))
          (venv1 (extend-varenv 'x '(:array (:vector int 2)) venv))
          (aenv1 (extend-appenv '#4#
                                '((:array (:vector int 2)) int (:vector int 2))
                  (extend-appenv '#5# '(int int (:vector int 2))
                   aenv))))
      (is (compile-setf form venv1 aenv1 fenv)
          '(setf (int2-aref* x0 0)
                 (the (values fixnum fixnum) (int2-values* 1 1)))
          "Base case - array type of int2 type."))))


;;
;; COMPILE-LET

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'compile-let)
        #'avm.lang.compiler.lisp.compile::compile-let))

(subtest "compile-let"

  (with-env (aenv fenv venv)
    (let ((form '(let (#1=(x 1)) x))
          (aenv1 (extend-appenv '#1# 'int aenv)))
      (is (compile-let form venv aenv1 fenv)
          '(let ((x0 1))
             (declare (ignorable x0))
             (declare (type fixnum x0))
             (declare (ignorable x0))
             x0)
          "Base case - scalar type.")))

  (with-env (aenv fenv venv)
    (let ((form '(let (#2=(x #3=(int2 1 1))) x))
          (aenv1 (extend-appenv '#2# '(:vector int 2)
                  (extend-appenv '#3# '(int int (:vector int 2))
                   aenv))))
      (is (compile-let form venv aenv1 fenv)
          '(multiple-value-bind (x0 x1)
              (the (values fixnum fixnum) (avm.lang.data::int2-values* 1 1))
             (declare (ignorable x0 x1))
             (declare (type fixnum x0 x1))
             (declare (ignorable x0 x1))
             (avm.lang.data::int2-values* x0 x1))
          "Base case - vector type.")))

  (with-env (aenv fenv venv)
    (let ((form '(let (#4=(x as)) x))
          (venv1 (extend-varenv 'as '(:array int) venv))
          (aenv1 (extend-appenv '#4# '(:array int) aenv)))
      (is (compile-let form venv1 aenv1 fenv)
          '(let ((x1 as0))
             (declare (ignorable x1))
             (declare (type int-array x1))
             (declare (ignorable x1))
             x1)
          "Base case - array type.")))

  (with-env (aenv fenv venv)
    (let ((form '(let (#5=(x 1)
                       #6=(y #7=(int2 1 1)))
                   x))
          (aenv1 (extend-appenv '#5# 'int
                  (extend-appenv '#6# '(:vector int 2)
                   (extend-appenv '#7# '(int int (:vector int 2))
                    aenv)))))
      (is (compile-let form venv aenv1 fenv)
          '(let ((x0 1))
             (declare (ignorable x0))
             (declare (type fixnum x0))
             (declare (ignorable x0))
             (multiple-value-bind (y1 y2)
                 (the (values fixnum fixnum) (avm.lang.data::int2-values* 1 1))
               (declare (ignorable y1 y2))
               (declare (type fixnum y1 y2))
               (declare (ignorable y1 y2))
               x0))
          "Base case - multiple bindings."))))


;;
;; COMPILE-FLET

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'compile-flet)
        #'avm.lang.compiler.lisp.compile::compile-flet))

(subtest "compile-flet"

  (with-env (aenv fenv venv)
    (let ((form '(flet (#1=(aux (x) x))
                   #2=(aux 1)))
          (aenv1 (extend-appenv '#1# '(int int)
                  (extend-appenv '#2# '(int int) aenv))))
      (is (compile-flet form venv aenv1 fenv)
          '(flet ((aux0 (x0)
                   (declare (optimize (speed 3) (safety 0)))
                   (declare (ignorable x0))
                   (declare (type fixnum x0))
                   x0))
             (let ((x1 1))
               (declare (type fixnum x1))
               (the fixnum (aux0 x1))))
          "Base case.")))

  (with-env (aenv fenv venv)
    (let ((form '(flet (#3=(aux (x y) x))
                   #4=(aux 1 2)))
          (aenv1 (extend-appenv '#3# '(int int int)
                  (extend-appenv '#4# '(int int int) aenv))))
      (is (compile-flet form venv aenv1 fenv)
          '(flet ((aux0 (x0 y1)
                    (declare (optimize (speed 3) (safety 0)))
                    (declare (ignorable x0 y1))
                    (declare (type fixnum x0))
                    (declare (type fixnum y1))
                    x0))
             (let ((x2 1))
               (declare (type fixnum x2))
               (let ((y3 2))
                 (declare (type fixnum y3))
                 (the fixnum (aux0 x2 y3)))))
          "Base case - multiple arguments."))))


;;
;; COMPILE-LABELS

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'compile-labels)
        #'avm.lang.compiler.lisp.compile::compile-labels))

(subtest "compile-labels"

  (with-env (aenv fenv venv)
    (let ((form '(labels (#1=(aux (x) (aux x)))
                   #2=(aux 1)))
          (aenv1 (extend-appenv '#1# '(int int)
                  (extend-appenv '#2# '(int int) aenv))))
      (is (compile-labels form venv aenv1 fenv)
          '(labels ((aux0 (x0)
                      (declare (optimize (speed 3) (safety 0)))
                      (declare (ignorable x0))
                      (declare (type fixnum x0))
                      (let ((x1 x0))
                        (declare (type fixnum x1))
                        (the fixnum (aux0 x1)))))
             (let ((x2 1))
               (declare (type fixnum x2))
               (the fixnum (aux0 x2))))
          "Base case.")))

  (with-env (aenv fenv venv)
    (let ((form '(labels (#3=(aux (x y) (aux x y)))
                   #4=(aux 1 2)))
          (aenv1 (extend-appenv '#3# '(int int int)
                  (extend-appenv '#4# '(int int int) aenv))))
      (is (compile-labels form venv aenv1 fenv)
          '(labels ((aux0 (x0 y1)
                      (declare (optimize (speed 3) (safety 0)))
                      (declare (ignorable x0 y1))
                      (declare (type fixnum x0))
                      (declare (type fixnum y1))
                      (let ((x2 x0))
                        (declare (type fixnum x2))
                        (let ((y3 y1))
                          (declare (type fixnum y3))
                          (the fixnum (aux0 x2 y3))))))
             (let ((x4 1))
               (declare (type fixnum x4))
               (let ((y5 2))
                 (declare (type fixnum y5))
                 (the fixnum (aux0 x4 y5)))))
          "Base case - multiple arguments."))))


;;
;; COMPILE-BUILT-IN-APPLY

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'compile-built-in-apply)
        #'avm.lang.compiler.lisp.compile::compile-built-in-apply))

(subtest "compile-built-in-apply"

  (with-env (aenv fenv venv)
    (let ((aenv1 (extend-appenv '#1=(+ 1 1) '(int int int) aenv)))
      (is (compile-built-in-apply '#1# venv aenv1 fenv)
          '(the fixnum (+ 1 1))
          "Base case - int addition.")))

  (with-env (aenv fenv venv)
    (let ((aenv1 (extend-appenv '#2=(coerce 1) '(int double) aenv)))
      (is (compile-built-in-apply '#2# venv aenv1 fenv)
          '(the double-float (avm.lang.compiler.lisp.built-in::int->double 1))
          "Base case - COERCE of int to double.")))

  (with-env (aenv fenv venv)
    (is-error (compile-built-in-apply '(+ 1 1) venv aenv fenv)
              simple-error
              "Not exist in appenv."))

  (with-env (aenv fenv venv)
    (let ((aenv1 (extend-appenv '#3=(+ 1 1 1) '(int int int) aenv)))
      (is-error (compile-built-in-apply '#3# venv aenv1 fenv)
                simple-error
                "Invalid number of arguments."))))


(finalize)
