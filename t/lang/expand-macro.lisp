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
                                    #'(lambda (args)
                                        (destructuring-bind (x) args
                                          x))
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
    (is (macro-p '(1) fenv)
        nil
        "Base case - not macro."))

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
;; EXPAND-MACRO-LITERAL

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'expand-macro-literal)
        #'avm.lang.expand-macro::expand-macro-literal))

(subtest "expand-macro-literal"

  (with-env (fenv)
    (is (expand-macro-literal 1 fenv)
        1
        "Base case.")))


;;
;; EXPAND-MACRO-REFERENCE

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'expand-macro-reference)
        #'avm.lang.expand-macro::expand-macro-reference))

(subtest "expand-macro-reference"

  (with-env (fenv)
    (is (expand-macro-reference 'x fenv)
        'x
        "Base case.")))


;;
;; EXPAND-MACRO-ACCESSOR

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'expand-macro-accessor)
        #'avm.lang.expand-macro::expand-macro-accessor))

(subtest "expand-macro-accessor"

  (with-env (fenv)
    (is (expand-macro-accessor '(int2-x (foo x)) fenv)
        '(int2-x x)
        "Base case.")))


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


;;
;; EXPAND-MACRO-IF

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'expand-macro-if)
        #'avm.lang.expand-macro::expand-macro-if))

(subtest "expand-macro-if"

  (with-env (fenv)
    (is (expand-macro-if '(if (foo (= 1 1)) 1 2) fenv)
        '(if (= 1 1) 1 2)
        "Base case - test form."))

  (with-env (fenv)
    (is (expand-macro-if '(if (= 1 1) (foo 1) 2) fenv)
        '(if (= 1 1) 1 2)
        "Base case - then form."))

  (with-env (fenv)
    (is (expand-macro-if '(if (= 1 1) 1 (foo 2)) fenv)
        '(if (= 1 1) 1 2)
        "Base case - else form.")))


;;
;; EXPAND-MACRO-LET

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'expand-macro-let)
        #'avm.lang.expand-macro::expand-macro-let))

(subtest "expand-macro-let"

  (with-env (fenv)
    (is (expand-macro-let '(let ((x (foo 1))) x) fenv)
        '(let ((x 1)) x)
        "Base case - macro in binding."))

  (with-env (fenv)
    (is (expand-macro-let '(let ((x 1)) (foo x) (foo 1)) fenv)
        '(let ((x 1)) x 1)
        "Base case - macro in body.")))


;;
;; EXPAND-MACRO-FLET

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'expand-macro-flet)
        #'avm.lang.expand-macro::expand-macro-flet))

(subtest "expand-macro-flet"

  (with-env (fenv)
    (is (expand-macro-flet '(flet ((aux () (foo 1) (foo 2)))
                              (aux))
                           fenv)
        '(flet ((aux () 1 2))
           (aux))
        "Base case - macro in binding."))

  (with-env (fenv)
    (is (expand-macro-flet '(flet ((aux () 1))
                              (foo 1)
                              (foo (aux 2)))
                           fenv)
        '(flet ((aux () 1))
           1 (aux 2))
        "Base case - macro in body.")))


;;
;; EXPAND-MACRO-LABELS

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'expand-macro-labels)
        #'avm.lang.expand-macro::expand-macro-labels))

(subtest "expand-macro-flet"

  (with-env (fenv)
    (is (expand-macro-labels '(labels ((aux () (foo 1) (foo 2)))
                                (aux))
                             fenv)
        '(labels ((aux () 1 2))
           (aux))
        "Base case - macro in binding."))

  (with-env (fenv)
    (is (expand-macro-labels '(labels ((aux () 1))
                                (foo 1)
                                (foo (aux 2)))
                             fenv)
        '(labels ((aux () 1))
           1 (aux 2))
        "Base case - macro in body.")))


;;
;; EXPAND-MACRO-SETF

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'expand-macro-setf)
        #'avm.lang.expand-macro::expand-macro-setf))

(subtest "expand-macro-setf"

  (with-env (fenv)
    (is (expand-macro-setf '(setf (aref x (foo 0)) 1) fenv)
        '(setf (aref x 0) 1)
        "Base case - macro in array place."))

  (with-env (fenv)
    (is (expand-macro-setf '(setf (aref x 0) (foo 1)) fenv)
        '(setf (aref x 0) 1)
        "Base case - macro in new value.")))


;;
;; EXPAND-MACRO-APPLY

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'expand-macro-apply)
        #'avm.lang.expand-macro::expand-macro-apply))

(subtest "expand-macro-apply"

  (with-env (fenv)
    (is (expand-macro-apply '(bar (foo 1) (foo 2)) fenv)
        '(bar 1 2)
        "Base case.")))


(finalize)

