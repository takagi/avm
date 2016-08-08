#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm-test.api.macro
  (:use :cl
        :prove
        :avm
        :avm.api.macro))
(in-package :avm-test.api.macro)


(plan nil)


(defkernel-macro foo (x)
  `(bar ,x))

(defkernel-macro bar (x)
  x)


;;
;; EXPAND-MACRO-1

(subtest "expand-macro-1"

  (is-values (expand-macro-1 '(foo 1))
             '((bar 1) t)
             "Base case - macro expanded.")

  (is-values (expand-macro-1 1)
             '(1 nil)
             "Base case - macro not expanded."))


;;
;; EXPAND-MACRO

(subtest "expand-macro"

  (is-values (expand-macro '(foo 1))
             '(1 t)
             "Base case - macro expanded.")

  (is-values (expand-macro 1)
             '(1 nil)
             "Base case - macro not expanded."))


(finalize)
