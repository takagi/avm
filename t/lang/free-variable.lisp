#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm-test.lang.free-variable
  (:use :cl
        :prove
        :avm.lang.free-variable))
(in-package :avm-test.lang.free-variable)


(plan nil)


;;
;; CHECK-FREE-VARIABLE-LET

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'check-free-variable-let)
        #'avm.lang.free-variable::check-free-variable-let))

(subtest "check-free-variable-let"

  (is (check-free-variable-let '(let ((x 1)) x) nil nil)
      '(x)
      "Base case.")

  (is-error (check-free-variable-let '(let ((x y)) x) nil nil)
            simple-error
            "Base case - free variable in binding.")

  (is-error (check-free-variable-let '(let ((x 1)) y) nil nil)
            simple-error
            "Base case - free variable in body."))


;;
;; CHECK-FREE-VARIABLE-FLET

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'check-free-variable-flet)
        #'avm.lang.free-variable::check-free-variable-flet))

(subtest "check-free-variable-flet"

  (is (check-free-variable-flet '(flet ((foo () 1)) 1) nil nil)
      nil
      "Base case.")

  (is-error (check-free-variable-flet '(flet ((foo () x)) 1) nil nil)
            simple-error
            "Base case - free variable in binding.")

  (is-error (check-free-variable-flet '(flet ((foo () 1)) x) nil nil)
            simple-error
            "Base case - free variable in body."))


;;
;; CHECK-FREE-VARIABLE-LABELS

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'check-free-variable-labels)
        #'avm.lang.free-variable::check-free-variable-labels))

(subtest "check-free-variable-labels"

  (is (check-free-variable-labels '(labels ((foo () 1)) 1) nil nil)
      nil
      "Base case.")

  (is-error (check-free-variable-labels '(labels ((foo (x) x y)) 1) nil nil)
            simple-error
            "Base case - free variable in binding forms.")

  (is-error (check-free-variable-labels '(labels ((foo () 1)) x) nil nil)
            simple-error
            "Base case - free variable in body."))


(finalize)
