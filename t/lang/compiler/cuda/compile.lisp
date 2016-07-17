#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm-test.lang.compiler.cuda.compile
  (:use :cl
        :prove
        :avm
        :avm.lang.typenv
        :avm.lang.appenv
        :avm.lang.funenv
        ))
(in-package :avm-test.lang.compiler.cuda.compile)


(plan nil)

(defmacro with-env ((tenv aenv fenv) &body body)
  `(let ((,tenv (empty-typenv))
         (,aenv (empty-appenv))
         (,fenv (empty-funenv)))
     ,@body))

(setf (fdefinition 'compile-form)
      #'avm.lang.compiler.cuda.compile::compile-form)


(subtest "Literal"

  (with-env (tenv aenv fenv)
    (is-values (compile-form 1 :tail tenv aenv fenv nil)
               '(1 cl-cuda:int nil)
               "Int literal."))

  (with-env (tenv aenv fenv)
    (is-values (compile-form 1.0 :tail tenv aenv fenv nil)
               '(1.0 cl-cuda:float nil)
               "Float literal."))

  (with-env (tenv aenv fenv)
    (is-values (compile-form 1.0d0 :tail tenv aenv fenv nil)
               '(1.0d0 cl-cuda:double nil)
               "Double literal."))

  (with-env (tenv aenv fenv)
    (is-error (compile-form :foo :tail tenv aenv fenv nil)
              simple-error
              "Invalid form."))
  )

(subtest "Reference"

  (with-env (tenv aenv fenv)
    (let ((tenv1 (extend-typenv 'x 'int tenv)))
      (is (compile-form 'x :tail tenv1 aenv fenv nil)
          '(return x)
          "Reference at tail.")))

  (with-env (tenv aenv fenv)
    (let ((tenv1 (extend-typenv 'x 'int tenv)))
      (is (compile-form 'x '(:non-tail y) tenv1 aenv fenv nil)
          '(set y x)
          "Reference at non-tail.")))

  (with-env (tenv aenv fenv)
    (is-error (compile-form :foo :tail tenv aenv fenv nil)
              simple-error
              "Invalid form."))

  (with-env (tenv aenv fenv)
    (is-error (compile-form 'x :tail tenv aenv fenv nil)
              simple-error
              "Invalid form - variable not found."))

  (with-env (tenv aenv fenv)
    (let ((tenv1 (extend-typenv 'x 'int tenv)))
      (is-error (compile-form 'x :foo tenv1 aenv fenv nil)
                simple-error
                "Invalid destination.")))
  )

(subtest "LET"
    
  (with-env (tenv aenv fenv)
    (is (compile-form '(let ((x 1)) x) :tail tenv aenv fenv nil)
        '(let ((x 0)) (set x 1) (return x))
        "LET form - int type."))

  (with-env (tenv aenv fenv)
    (is (compile-form '(let ((x 1.0)) x) :tail tenv aenv fenv nil)
        '(let ((x 0.0)) (set x 1.0) (return x))
        "LET form - float type."))

  (with-env (tenv aenv fenv)
    (is (compile-form '(let ((x (let ((y 1)) y))) x) :tail tenv aenv fenv nil)
        '(let ((x 0)) (let ((y 0)) (set y 1) (set x y)) (return x))
        "Nested LET form."))

  (with-env (tenv aenv fenv)
    (let ((tenv1 (extend-typenv 'b 'bool tenv)))
      (is (compile-form '(let ((x (if b 1 0))) x) :tail tenv1 aenv fenv nil)
          '(let ((x 0))
             (if b (set x 1) (set x 0))
             (return x))
          "LET form with IF form in binding.")))
  )


(finalize)
