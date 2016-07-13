#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.samples.vector-add
  (:use :cl
        :avm)
  (:export :main))
(in-package :avm.samples.vector-add)


(defkernel vector-add (c a b)
  (set (aref c i) (the int (+ (aref a i) (aref b i)))))

(defun random-init (array n)
  (dotimes (i n)
    (setf (array-aref array i) (random 100))))

(defun verify-result (as bs cs n)
  (dotimes (i n)
    (let ((a (array-aref as i))
          (b (array-aref bs i))
          (c (array-aref cs i)))
      (unless (= (+ a b) c)
        (error "Verification failed: i=~A, a=~A, b=~A, c=~A" i a b c))))
  (format t "Successfully verified.~%"))

(defun main (n &optional dev-id)
  (with-cuda (dev-id)
    (with-arrays ((a int n)
                  (b int n)
                  (c int n))
      (random-init a n)
      (random-init b n)
      (time
       (vector-add c a b))
      (verify-result a b c n))))
