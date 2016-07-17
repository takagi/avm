#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm-test.api.array
  (:use :cl
        :prove
        :avm))
(in-package :avm-test.api.array)


(plan nil)

(subtest "array-aref"

  (with-arrays ((xs float4 1))
    (setf (array-aref xs 0) (values 1.0 2.0 3.0 4.0))
    (is-values (array-aref xs 0)
               '(1.0 2.0 3.0 4.0)))
  )
               


(subtest "sync-array"

  (with-cuda (0)
    (with-arrays ((xs float4 (* 1024 1024))
                  (ys float4 (* 1024 1024)))
      ;; Initialize array.
      (dotimes (i (array-size xs))
        (setf (array-aref xs i)
              (values (random 1.0) (random 1.0) (random 1.0) (random 1.0))))
      ;; Copy to another for verification.
      (dotimes (i (array-size xs))
        (setf (array-aref ys i) (array-aref xs i)))
      ;; Synchroize from Lisp to CUDA.
      (avm.api.array::sync-array xs :lisp :cuda)
      ;; Clear array.
      (dotimes (i (array-size xs))
        (setf (array-aref xs i) (values 0.0 0.0 0.0 0.0)))
      ;; Synchroize back from CUDA to Lisp.
      (avm.api.array::sync-array xs :cuda :lisp)
      ;; Verify arrays.
      (dotimes (i (array-size xs))
        (assert (= (array-aref ys i) (array-aref xs i))))
      (ok t)))
  )


(finalize)
