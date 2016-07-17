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

(subtest "SET"

  (is (k-normal '(set x 1))
      '(set x 1))

  (is (k-normal '(set (int2-x x) 1))
      '(set (int2-x x) 1))

  (is (k-normal '(set (aref x 0) 1))
      '(set (aref x 0) 1))

  (is (k-normal '(set (int2-x (aref x 0)) 1))
      '(set (int2-x (aref x 0)) 1))

  (let ((*gentmp-counter* 0))
    (is (k-normal '(set (aref x (+ i 1)) 1))
        '(let ((t0 (+ i 1)))
          (set (aref x t0) 1))))

  (let ((*gentmp-counter* 0))
    (is (k-normal '(set (int2-x (aref x (+ i 1))) 1))
        '(let ((t0 (+ i 1)))
          (set (int2-x (aref x t0)) 1))))

  (is (k-normal '(set x y))
      '(set x y))

  (let ((*gentmp-counter* 0))
    (is (k-normal '(set x (+ i 1)))
        '(let ((t0 (+ i 1)))
          (set x t0))))
  )


(finalize)
