#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm-test.lang.expand-macro
  (:use :cl
        :prove
        :avm
        :avm.lang.funenv
        :avm.lang.expand-macro))
(in-package :avm-test.lang.expand-macro)


(plan nil)

(defmacro with-env ((var) &body body)
  `(let ((,var (extend-funenv-macro 'foo '(x) '(x)
                (empty-funenv))))
     ,@body))


;;
;; MACRO-P

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'macro-p)
        #'avm.lang.expand-macro::macro-p))

(subtest "macro-p"

  (with-env (fenv)
    (is (macro-p 1 fenv)
        nil
        "Base case - not list."))

  (with-env (fenv)
    (is (macro-p '(foo 1) fenv)
        t
        "Base case - macro."))

  (with-env (fenv)
    (is (macro-p '(bar 1) fenv)
        nil
        "Base case - not macro."))

  (with-env (fenv)
    (is-error (macro-p '(1) fenv)
              type-error
              "Invalid form."))

  (is-error (macro-p '(foo 1) :foo)
            type-error
            "Invalid funenv."))


;;
;; EXPAND-MACRO-MACRO

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'expand-macro-macro)
        #'avm.lang.expand-macro::expand-macro-macro))

(subtest "expand-macro-macro"

    (with-env (fenv)
      (is (expand-macro-macro '(foo 1) fenv)
          1
          "Base case."))

    (with-env (fenv)
      (is (expand-macro-macro '(foo (foo 1)) fenv)
          1
          "Base case - nested macro.")))


;;
;; EXPAND-MACRO-THE

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'expand-macro-the)
        #'avm.lang.expand-macro::expand-macro-the))

(subtest "expand-macro-the"

  (with-env (fenv)
    (is (expand-macro-the '(the int (foo 1)) fenv)
        '(the int 1)
        "Base case.")))


(finalize)

