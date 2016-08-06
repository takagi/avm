#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm-test.lang.convert-functions
  (:use :cl
        :prove
        :avm
        :avm.lang.convert-functions))
(in-package :avm-test.lang.convert-functions)


(plan nil)


;;
;; CONVERT-LET

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'convert-let)
        #'avm.lang.convert-functions::convert-let))

(subtest "convert-let"

  (is (convert-let '(let ((x (foo 1))) x))
      '(let ((x (foo i n 1)))
         x)
      "Base case - convert binding.")

  (is (convert-let '(let ((x 1)) (foo x)))
      '(let ((x 1))
         (foo i n x))
      "Base case - convert body.")

  (is (convert-let '(let ((x 1)) (foo x) (bar x)))
      '(let ((x 1))
         (foo i n x)
         (bar i n x))
      "Base case - convert multiple body forms."))


;;
;; CONVERT-FLET

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'convert-flet)
        #'avm.lang.convert-functions::convert-flet))

(subtest "convert-flet"

  (is (convert-flet '(flet ((aux (x) (foo 1)))
                       1))
      '(flet ((aux (i n x) (foo i n 1)))
         1)
      "Base case - convert binding.")

  (is (convert-flet '(flet ((aux (x) (foo 1) (bar 1)))
                       1))
      '(flet ((aux (i n x) (foo i n 1) (bar i n 1)))
         1)
      "Base case - convert multiple binding forms.")

  (is (convert-flet '(flet ((aux (x) x))
                       (foo 1)))
      '(flet ((aux (i n x) x))
         (foo i n 1))
      "Base case - convert body.")

  (is (convert-flet '(flet ((aux (x) x))
                       (foo 1)
                       (bar 1)))
      '(flet ((aux (i n x) x))
         (foo i n 1)
         (bar i n 1))
      "Base case - convert multiple body forms."))


;;
;; CONVERT-LABELS

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'convert-labels)
        #'avm.lang.convert-functions::convert-labels))

(subtest "convert-labels"

  (is (convert-labels '(labels ((aux (x) (foo 1)))
                         1))
      '(labels ((aux (i n x) (foo i n 1)))
         1)
      "Base case - convert binding.")

  (is (convert-labels '(labels ((aux (x) (foo 1) (bar 1)))
                         1))
      '(labels ((aux (i n x) (foo i n 1) (bar i n 1)))
         1)
      "Base case - convert multiple binding forms.")

  (is (convert-labels '(labels ((aux (x) x))
                         (foo 1)))
      '(labels ((aux (i n x) x))
         (foo i n 1))
      "Base case - convert body.")

  (is (convert-labels '(labels ((aux (x) x))
                         (foo 1)
                         (bar 1)))
      '(labels ((aux (i n x) x))
         (foo i n 1)
         (bar i n 1))
      "Base case - convert multiple body forms."))


(finalize)
