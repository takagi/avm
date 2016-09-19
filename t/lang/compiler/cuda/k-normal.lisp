#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm-test.lang.compiler.cuda.k-normal
  (:use :cl
        :prove
        :avm
        :avm.lang.compiler.cuda.k-normal
        ))
(in-package :avm-test.lang.compiler.cuda.k-normal)


(plan nil)

(subtest "SETF"

  (is (k-normal '(setf x 1))
      '(setf x 1))

  (is (k-normal '(setf (int2-x x) 1))
      '(setf (int2-x x) 1))

  (is (k-normal '(setf (aref x 0) 1))
      '(setf (aref x 0) 1))

  (is (k-normal '(setf (int2-x (aref x 0)) 1))
      '(setf (int2-x (aref x 0)) 1))

  (let ((*gentmp-counter* 0))
    (is (k-normal '(setf (aref x (+ i 1)) 1))
        '(let ((t0 (+ i 1)))
          (setf (aref x t0) 1))))

  (let ((*gentmp-counter* 0))
    (is (k-normal '(setf (int2-x (aref x (+ i 1))) 1))
        '(let ((t0 (+ i 1)))
          (setf (int2-x (aref x t0)) 1))))

  (is (k-normal '(setf x y))
      '(setf x y))

  (let ((*gentmp-counter* 0))
    (is (k-normal '(setf x (+ i 1)))
        '(let ((t0 (+ i 1)))
          (setf x t0))))
  )


(finalize)
