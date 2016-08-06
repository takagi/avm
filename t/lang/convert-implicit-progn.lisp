#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm-test.lang.convert-implicit-progn
  (:use :cl
        :prove
        :avm
        :avm-test.util
        :avm.lang.convert-implicit-progn))
(in-package :avm-test.lang.convert-implicit-progn)


(plan nil)


;;
;; CONVERT-ACCESSOR

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'convert-accessor)
        #'avm.lang.convert-implicit-progn::convert-accessor))

(subtest "convert-accessor"

  (is (replace-gensym
       (convert-accessor '(int2 (let ((x 1)) 1 2)
                                  1)))
      '(int2 (let ((x 1))
               (let ((_ 1))
                 2))
             1)
      "Base case."))


;;
;; CONVERT-THE

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'convert-the)
        #'avm.lang.convert-implicit-progn::convert-the))

(subtest "convert-the"

  (is (replace-gensym
       (convert-the '(the int (let ((x 1)) 1 2))))
      '(the int (let ((x 1))
                  (let ((_ 1))
                    2)))
      "Base case."))


;;
;; CONVERT-IF

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'convert-if)
        #'avm.lang.convert-implicit-progn::convert-if))

(subtest "convert-if"

  (is (replace-gensym
       (convert-if '(if (let ((x 1)) (= 1 1) (= 2 2))
                        1 2)))
      '(if (let ((x 1))
             (let ((_ (= 1 1)))
               (= 2 2)))
           1 2)
      "Base case - test form.")

  (is (replace-gensym
       (convert-if '(if (= 1 1)
                        (let ((x 1))
                          1 2)
                        2)))
      '(if (= 1 1)
           (let ((x 1))
             (let ((_ 1))
               2))
           2)
      "Base case - then form.")

  (is (replace-gensym
       (convert-if '(if (= 1 1)
                        1
                        (let ((x 1))
                          1 2))))
      '(if (= 1 1)
           1
           (let ((x 1))
             (let ((_ 1))
               2)))
      "Base case - else form."))


;;
;; CONVERT-LET

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'convert-let)
        #'avm.lang.convert-implicit-progn::convert-let))

(subtest "convert-let"

  (is (replace-gensym
       (convert-let '(let ((x 1)) x x)))
      '(let ((x 1))
         (let ((_ x))
           x))
      "Base case.")

  (is (replace-gensym
       (convert-let '(let ((x (let ((x 1))
                                x x)))
                       x x)))
      '(let ((x (let ((x 1))
                  (let ((_ x))
                    x))))
        (let ((_ x))
          x))
      "Base case - another let in binding.")

  (is (replace-gensym
       (convert-let '(let ((x 1))
                       (let ((x 1))
                         x x)
                       x)))
      '(let ((x 1))
         (let ((_ (let ((x 1))
                    (let ((_ x))
                      x))))
           x))
      "Base case - another let in body."))


;;
;; CONVERT-FLET

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'convert-flet)
        #'avm.lang.convert-implicit-progn::convert-flet))

(subtest "convert-flet"

  (is (replace-gensym
       (convert-flet '(flet ((aux ()
                               1 2))
                        1 2)))
      '(flet ((aux ()
                (let ((_ 1))
                  2)))
         (let ((_ 1))
           2))
      "Base case."))


;;
;; CONVERT-LABELS

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'convert-labels)
        #'avm.lang.convert-implicit-progn::convert-labels))

(subtest "convert-labels"

  (is (replace-gensym
       (convert-labels '(labels ((aux ()
                                   1 2))
                          1 2)))
      '(labels ((aux ()
                 (let ((_ 1))
                   2)))
         (let ((_ 1))
           2))
      "Base case."))


;;
;; CONVERT-SET

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'convert-set)
        #'avm.lang.convert-implicit-progn::convert-set))

(subtest "convert-set"

  (is (replace-gensym
       (convert-set '(set x (let ((x 1))
                              1 2))))
      '(set x (let ((x 1))
                (let ((_ 1))
                  2)))
      "Base case.")

  (is (replace-gensym
       (convert-set '(set (aref x (let ((x 1))
                                    1 2))
                          1)))
      '(set (aref x (let ((x 1))
                      (let ((_ 1))
                        2)))
            1)
      "Base case - LET form in array place."))


;;
;; CONVERT-APPLY

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'convert-apply)
        #'avm.lang.convert-implicit-progn::convert-apply))

(subtest "convert-apply"

  (is (replace-gensym
       (convert-apply '(foo (let ((x 1)) 1 2))))
      '(foo (let ((x 1))
              (let ((_ 1))
                2)))
      "Base case."))


(finalize)
