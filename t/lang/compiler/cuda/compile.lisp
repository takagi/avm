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

(defmacro with-env ((aenv fenv) &body body)
  `(let ((,aenv (empty-appenv))
         (,fenv (empty-funenv)))
     ,@body))


;;
;; COMPILE-LITERAL

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'compile-literal)
        #'avm.lang.compiler.cuda.compile::compile-literal))

(subtest "compile-literal"

  (with-env (aenv fenv)
    (is-values (compile-literal 1 :tail aenv fenv nil)
               '((return 1) nil)
               "Base case - int literal, tail."))

  (with-env (aenv fenv)
    (is-values (compile-literal 1 '(:non-tail x) aenv fenv nil)
               '((set x 1) nil)
               "Base case - int literal, non-tail."))

  (with-env (aenv fenv)
    (is-values (compile-literal 1.0 :tail aenv fenv nil)
               '((return 1.0) nil)
               "Base case - float literal, tail."))

  (with-env (aenv fenv)
    (is-values (compile-literal 1.0 '(:non-tail x) aenv fenv nil)
               '((set x 1.0) nil)
               "Base case - float literal, non-tail."))

  (with-env (aenv fenv)
    (is-values (compile-literal 1.0d0 :tail aenv fenv nil)
               '((return 1.0d0) nil)
               "Base case - double literal, tail."))

  (with-env (aenv fenv)
    (is-values (compile-literal 1.0d0 '(:non-tail x) aenv fenv nil)
               '((set x 1.0d0) nil)
               "Base case - double literal, non-tail."))

  (with-env (aenv fenv)
    (is-error (compile-literal 1 :foo aenv fenv nil)
              simple-error
              "Invalid destination.")))


;;
;; COMPILE-REFERENCE

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'compile-reference)
        #'avm.lang.compiler.cuda.compile::compile-reference))

(subtest "compile-reference"

  (with-env (aenv fenv)
    (is-values (compile-reference 'x :tail aenv fenv nil)
               '((return x) nil)
               "Base case - tail."))

  (with-env (aenv fenv)
    (is-values (compile-reference 'x '(:non-tail y) aenv fenv nil)
               '((set y x) nil)
               "Base case - non-tail."))

  (with-env (aenv fenv)
    (is-error (compile-reference 'x :foo aenv fenv nil)
              simple-error
              "Invalid destination.")))


;;
;; COMPILE-LET

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'compile-let)
        #'avm.lang.compiler.cuda.compile::compile-let))

(subtest "compile-let"

  (with-env (aenv fenv)
    (let ((form '(let (#1=(x 1)) x))
          (aenv1 (extend-appenv '#1# 'int aenv)))
      (is-values (compile-let form :tail aenv1 fenv nil)
                 '((let ((x 0)) (set x 1) (return x)) nil)
                 "Base case - int value, tail.")))

  (with-env (aenv fenv)
    (let ((form '(let (#2=(x 1)) x))
          (aenv1 (extend-appenv '#2# 'int aenv)))
      (is-values (compile-let form '(:non-tail y) aenv1 fenv nil)
                 '((let ((x 0)) (set x 1) (set y x)) nil)
                 "Base case - int value, non-tail.")))

  (with-env (aenv fenv)
    (let ((form '(let (#3=(x 1.0)) x))
          (aenv1 (extend-appenv '#3# 'float aenv)))
      (is-values (compile-let form :tail aenv1 fenv nil)
                 '((let ((x 0.0)) (set x 1.0) (return x)) nil)
                 "Base case - float value, tail.")))

  (with-env (aenv fenv)
    (let ((form '(let (#4=(x 1.0)) x))
          (aenv1 (extend-appenv '#4# 'float aenv)))
      (is-values (compile-let form '(:non-tail y) aenv1 fenv nil)
                 '((let ((x 0.0)) (set x 1.0) (set y x)) nil)
                 "Base case - float value, non-tail.")))

  (with-env (aenv fenv)
    (let ((form '(let (#5=(x (let (#6=(y 1))
                               y)))
                   x))
          (aenv1 (extend-appenv '#5# 'int
                  (extend-appenv '#6# 'int
                   aenv))))
      (is-values (compile-let form :tail aenv1 fenv nil)
                 '((let ((x 0))
                     (let ((y 0))
                       (set y 1)
                       (set x y))
                     (return x))
                   nil)
                 "Base case - nested LET forms.")))

  (with-env (aenv fenv)
    (let ((form '(let (#7=(x (if b 1 0)))
                   x))
          (aenv1 (extend-appenv '#7# 'int aenv)))
      (is-values (compile-let form :tail aenv1 fenv nil)
                 '((let ((x 0))
                     (if b
                         (set x 1)
                         (set x 0))
                     (return x))
                   nil)
                 "Base case - with IF form in binding."))))


(finalize)
