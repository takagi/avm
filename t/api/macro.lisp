#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm-test.api.macro
  (:use :cl
        :prove
        :avm
        :avm.api.macro
        :avm-test.util))
(in-package :avm-test.api.macro)


(plan nil)


;;
;; EXPAND-MACRO-1

(defkernel-macro foo (x)
  `(bar ,x))

(defkernel-macro bar (x)
  x)

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


;;
;; PROGN

(subtest "progn"

  (is (replace-gensym
       (expand-macro '(progn 1 2)))
      '(let ((_ 1))
         (progn 2))
      "Base case - two forms.")

  (is (replace-gensym
       (expand-macro '(progn 1)))
      '1
      "Base case - a form.")

  (is-error (expand-macro '(progn))
            simple-error
            "Invalid form."))


;;
;; LET*

(subtest "let*"

  (is (expand-macro-1 '(let* ((x 1)
                              (y 1))
                         (+ x y)))
      '(let ((x 1))
         (let* ((y 1))
           (+ x y)))
      "Base case.")

  (is (expand-macro-1 '(let* ()
                         (+ x y)))
      '(progn (+ x y))
      "Base case - no bindings.")

  (is-error (expand-macro '(let* ()))
            simple-error
            "Invalid form."))


(finalize)
