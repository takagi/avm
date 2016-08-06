#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm-test.lang.convert-implicit-progn
  (:use :cl
        :prove
        :avm-test.util
        :avm.lang.convert-implicit-progn))
(in-package :avm-test.lang.convert-implicit-progn)


(plan nil)


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
  (setf (fdefinition #'convert-flet)
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
;; CONVERT-IMPLICIT-PROGN-LABELS

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition #'convert-labels)
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


(finalize)
