#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm-test.api.array
  (:use :cl
        :prove
        :avm
        :avm.api.array))
(in-package :avm-test.api.array)


(plan nil)


;;
;; Array

(subtest "array-cuda-available-on-allocation-p"

  (with-cuda (0)
    (with-array (xs int 1)
      (ok (avm.api.array::array-cuda-available-on-allocation-p xs)
          "CUDA available on allocation.")))

  (with-array (xs int 1)
    (ok (not (avm.api.array::array-cuda-available-on-allocation-p xs))
        "CUDA not available on allocation.")))

(subtest "array-freed-p"

  (let ((xs (alloc-array 'int 1)))
    (ok (not (avm.api.array::array-freed-p xs)))
    (free-array xs)
    (ok (avm.api.array::array-freed-p xs)))

  (with-cuda (0)
    (let ((xs (alloc-array 'int 1)))
      (ok (not (avm.api.array::array-freed-p xs)))
      (free-array xs)
      (ok (avm.api.array::array-freed-p xs)))))

(subtest "check-cuda-available"

  (with-cuda (0)
    (ok (null (avm.api.array::check-cuda-available))
        "CUDA available."))

  (is-error (avm.api.array::check-cuda-available)
            simple-error
            "CUDA not available."))

(subtest "check-array-cuda-available-on-allocation"

  (with-cuda (0)
    (with-array (xs int 1)
      (ok (null (avm.api.array::check-array-cuda-available-on-allocation xs))
          "CUDA available on array allocation.")))

  (with-array (xs int 1)
    (is-error (avm.api.array::check-array-cuda-available-on-allocation xs)
              simple-error
              "CUDA not available on array allocation.")))

(subtest "check-array-not-freed"

  (let ((xs (alloc-array 'int 1)))
    (ok (null (avm.api.array::check-array-not-freed xs))
        "Array not freed."))

  (let ((xs (alloc-array 'int 1)))
    (free-array xs)
    (is-error (avm.api.array::check-array-not-freed xs)
              simple-error
              "Array already freed.")))

(subtest "array-tuple-array"

  (with-array (xs int 1)
    (ok (avm.api.array::array-tuple-array xs)
        "Tuple array in array not freed."))

  (let ((xs (alloc-array 'int 1)))
    (free-array xs)
    (is-error (avm.api.array::array-tuple-array xs)
              simple-error
              "Tuple array in array freed.")))

(subtest "array-host-ptr"

  (with-cuda (0)
    (with-array (xs int 1)
      (ok (avm.api.array::array-host-ptr xs)
          "Host pointer.")))

  (with-array (xs int 1)
    (is-error (avm.api.array::array-host-ptr xs)
              simple-error
              "CUDA not available."))

  (with-array (xs int 1)
    (with-cuda (0)
      (is-error (avm.api.array::array-host-ptr xs)
                simple-error
                "CUDA not available on allocation.")))

  (with-cuda (0)
    (let ((xs (alloc-array 'int 1)))
      (free-array xs)
      (is-error (avm.api.array::array-host-ptr xs)
                simple-error
                "Array already freed.")))

  (is-error (avm.api.array::array-host-ptr :foo)
            type-error
            "Invalid array."))

(subtest "array-device-ptr"

  (with-cuda (0)
    (with-array (xs int 1)
      (ok (avm.api.array::array-device-ptr xs)
          "Device pointer.")))

  (with-array (xs int 1)
    (is-error (avm.api.array::array-device-ptr xs)
              simple-error
              "CUDA not available."))

  (with-array (xs int 1)
    (with-cuda (0)
      (is-error (avm.api.array::array-device-ptr xs)
                simple-error
                "CUDA not available on allocation.")))

  (with-cuda (0)
    (let ((xs (alloc-array 'int 1)))
      (free-array xs)
      (is-error (avm.api.array::array-device-ptr xs)
                simple-error
                "Array already freed.")))

  (is-error (avm.api.array::array-device-ptr :foo)
            type-error
            "Invalid array."))

(subtest "array-lisp-up-to-date-p"

  (with-cuda (0)
    (with-array (xs int 1)
      (ok (eq t (avm.api.array::array-lisp-up-to-date-p xs))
          "Lisp memory in array up-to-date.")))

  (with-array (xs int 1)
    (is-error (avm.api.array::array-lisp-up-to-date-p xs)
              simple-error
              "CUDA not available."))

  (with-array (xs int 1)
    (with-cuda (0)
      (is-error (avm.api.array::array-lisp-up-to-date-p xs)
                simple-error
                "CUDA not available on allocation.")))

  (with-cuda (0)
    (let ((xs (alloc-array 'int 1)))
      (free-array xs)
      (is-error (avm.api.array::array-lisp-up-to-date-p xs)
                simple-error
                "Array already freed.")))

  (is-error (avm.api.array::array-lisp-up-to-date-p :foo)
            type-error
            "Invalid array."))

(subtest "array-cuda-up-to-date-p"

  (with-cuda (0)
    (with-array (xs int 1)
      (ok (eq t (avm.api.array::array-cuda-up-to-date-p xs))
          "CUDA memory in array up-to-date.")))

  (with-array (xs int 1)
    (is-error (avm.api.array::array-cuda-up-to-date-p xs)
              simple-error
              "CUDA not available."))

  (with-array (xs int 1)
    (with-cuda (0)
      (is-error (avm.api.array::array-cuda-up-to-date-p xs)
                simple-error
                "CUDA not available on allocation.")))

  (with-cuda (0)
    (let ((xs (alloc-array 'int 1)))
      (free-array xs)
      (is-error (avm.api.array::array-cuda-up-to-date-p xs)
                simple-error
                "Array already freed.")))

  (is-error (avm.api.array::array-cuda-up-to-date-p :foo)
            type-error
            "Invalid array."))

(subtest "alloc-array"

  (with-cuda (0)
    (with-array (xs int 1)
      (ok (and (avm.api.array::array-%tuple-array xs)
               (avm.api.array::array-%host-ptr xs)
               (avm.api.array::array-%device-ptr xs)
               (eql t (avm.api.array::array-%lisp-up-to-date xs))
               (eql t (avm.api.array::array-%cuda-up-to-date xs)))
          "Array allocated with CUDA available.")))

  (with-array (xs int 1)
    (ok (and (avm.api.array::array-%tuple-array xs)
             (null (avm.api.array::array-%host-ptr xs))
             (null (avm.api.array::array-%device-ptr xs))
             (eql :%lisp-up-to-date
                  (avm.api.array::array-%lisp-up-to-date xs))
             (eql :%cuda-up-to-date
                  (avm.api.array::array-%cuda-up-to-date xs)))
        "Array allocated with CUDA not available."))

  (with-array (xs int 1)
    (is (array-base-type xs)
        'int
        "Base type int.")))

(subtest "free-array"

  ;; CUDA-AVAILABLE-P                     : T
  ;; ARRAY-CUDA-AVAILABLE-ON-ALLOCATION-P : T
  ;; ARRAY-FREED-P                        : NIL
  (with-cuda (0)
    (let ((xs (alloc-array 'int 1)))
      (free-array xs)
      (ok (and (null (avm.api.array::array-%tuple-array xs))
               (cffi:null-pointer-p (avm.api.array::array-%host-ptr xs))
               (= 0 (avm.api.array::array-%device-ptr xs))
               (eql :%lisp-up-to-date
                    (avm.api.array::array-%lisp-up-to-date xs))
               (eql :%cuda-up-to-date
                    (avm.api.array::array-%cuda-up-to-date xs)))
          "Free array CUDA availabe on allocation with CUDA availabe.")))

  ;; CUDA-AVAILABLE-P                     : NIL
  ;; ARRAY-CUDA-AVAILABLE-ON-ALLOCATION-P : T
  ;; ARRAY-FREED-P                        : NIL
  (let (xs)
    (with-cuda (0)
      (setf xs (alloc-array 'int 1)))
    (is-error
     (free-array xs)
     simple-error
     "Not free array CUDA availabe on allocation with not CUDA availabe."))

  ;; CUDA-AVAILABLE-P                     : T
  ;; ARRAY-CUDA-AVAILABLE-ON-ALLOCATION-P : NIL
  ;; ARRAY-FREED-P                        : NIL
  (let ((xs (alloc-array 'int 1)))
    (with-cuda (0)
      (free-array xs))
    (ok (and (null (avm.api.array::array-%tuple-array xs))
             (null (avm.api.array::array-%host-ptr xs))
             (null (avm.api.array::array-%device-ptr xs))
             (eql :%lisp-up-to-date
                  (avm.api.array::array-%lisp-up-to-date xs))
             (eql :%cuda-up-to-date
                  (avm.api.array::array-%cuda-up-to-date xs)))
        "Free array CUDA not availabe on allocation with CUDA availabe."))

  ;; CUDA-AVAILABLE-P                     : NIL
  ;; ARRAY-CUDA-AVAILABLE-ON-ALLOCATION-P : NIL
  ;; ARRAY-FREED-P                        : NIL
  (let ((xs (alloc-array 'int 1)))
    (free-array xs)
    (ok (and (null (avm.api.array::array-%tuple-array xs))
             (null (avm.api.array::array-%host-ptr xs))
             (null (avm.api.array::array-%device-ptr xs))
             (eql :%lisp-up-to-date
                  (avm.api.array::array-%lisp-up-to-date xs))
             (eql :%cuda-up-to-date
                  (avm.api.array::array-%cuda-up-to-date xs)))
        "Free array not CUDA availabe on allocation with not CUDA availabe."))

  ;; CUDA-AVAILABLE-P                     : any
  ;; ARRAY-CUDA-AVAILABLE-ON-ALLOCATION-P : any
  ;; ARRAY-FREED-P                        : T
  (let ((xs (alloc-array 'int 1)))
    (free-array xs)
    (ok (null (free-array xs))
        "Array already freed."))

  (is-error (free-array :foo)
            type-error
            "Invalid array."))

(defkernel fill-ones (xs)
  (setf (aref xs i) 1))

(subtest "array-aref"

  ;; CUDA-AVAILABLE-P                     : T
  ;; ARRAY-CUDA-AVAILABLE-ON-ALLOCATION-P : T
  ;; ARRAY-FREED-P                        : NIL
  (with-cuda (0)
    (with-array (xs int 1)
      (setf (array-aref xs 0) 0)
      (fill-ones xs)
      (is (array-aref xs 0)
          1
          "Array CUDA availabe on allocation with CUDA availabe.")))

  ;; CUDA-AVAILABLE-P                     : NIL
  ;; ARRAY-CUDA-AVAILABLE-ON-ALLOCATION-P : T
  ;; ARRAY-FREED-P                        : NIL
  (let (xs)
    (with-cuda (0)
      (setf xs (alloc-array 'int 1)))
    (is-error (array-aref xs 0)
              simple-error
              "Array CUDA availabe on allocation with not CUDA availabe."))

  ;; CUDA-AVAILABLE-P                     : T
  ;; ARRAY-CUDA-AVAILABLE-ON-ALLOCATION-P : NIL
  ;; ARRAY-FREED-P                        : NIL
  (with-array (xs int 1)
    (with-cuda (0)
      (is-error (array-aref xs 0)
                simple-error
                "Array not CUDA availabe on allocation with CUDA availabe.")))

  ;; CUDA-AVAILABLE-P                     : NIL
  ;; ARRAY-CUDA-AVAILABLE-ON-ALLOCATION-P : NIL
  ;; ARRAY-FREED-P                        : NIL
  (with-array (xs int 1)
    (setf (array-aref xs 0) 1)
    (is (array-aref xs 0)
        1
        "Array not CUDA availabe on allocation with not CUDA availabe."))

  ;; CUDA-AVAILABLE-P                     : any
  ;; ARRAY-CUDA-AVAILABLE-ON-ALLOCATION-P : any
  ;; ARRAY-FREED-P                        : T
  (let (xs)
    (setf xs (alloc-array 'int 1))
    (free-array xs)
    (is-error (array-aref xs 0)
              simple-error
              "Array already freed."))

  (is-error (array-aref :foo 0)
            type-error
            "Invalid array.")

  (with-array (xs int 1)
    (is-error (array-aref xs :foo)
              error
              "Invalid index.")))

(subtest "setf array-aref"

  ;; CUDA-AVAILABLE-P                     : T
  ;; ARRAY-CUDA-AVAILABLE-ON-ALLOCATION-P : T
  ;; ARRAY-FREED-P                        : NIL
  (with-cuda (0)
    (with-array (xs int 1)
      (fill-ones xs)
      (setf (array-aref xs 0) 2)
      (is (array-aref xs 0)
          2)
      (ok (avm.api.array::array-%lisp-up-to-date xs))
      (ok (not (avm.api.array::array-%cuda-up-to-date xs)))))

  ;; CUDA-AVAILABLE-P                     : NIL
  ;; ARRAY-CUDA-AVAILABLE-ON-ALLOCATION-P : T
  ;; ARRAY-FREED-P                        : NIL
  (let (xs)
    (with-cuda (0)
      (setf xs (alloc-array 'int 1)))
    (is-error (setf (array-aref xs 0) 1)
              simple-error
              "Array CUDA availabe on allocation with not CUDA availabe."))

  ;; CUDA-AVAILABLE-P                     : T
  ;; ARRAY-CUDA-AVAILABLE-ON-ALLOCATION-P : NIL
  ;; ARRAY-FREED-P                        : NIL
  (with-array (xs int 1)
    (with-cuda (0)
      (is-error (setf (array-aref xs 0) 1)
                simple-error
                "Array not CUDA availabe on allocation with CUDA availabe.")))

  ;; CUDA-AVAILABLE-P                     : NIL
  ;; ARRAY-CUDA-AVAILABLE-ON-ALLOCATION-P : NIL
  ;; ARRAY-FREED-P                        : NIL
  (with-array (xs int 1)
    (setf (array-aref xs 0) 1)
    (is (array-aref xs 0)
        1
        "Array not CUDA availabe on allocation with not CUDA availabe."))

  ;; CUDA-AVAILABLE-P                     : any
  ;; ARRAY-CUDA-AVAILABLE-ON-ALLOCATION-P : any
  ;; ARRAY-FREED-P                        : T
  (let (xs)
    (setf xs (alloc-array 'int 1))
    (free-array xs)
    (is-error (setf (array-aref xs 0) 1)
              simple-error
              "Array already freed."))

  (with-array (xs int 1)
    (is-error (array-aref xs :foo)
              error
              "Invalid index.")))

(subtest "array-size"

  (with-array (xs int 100)
    (is (array-size xs)
        100))

  (let (xs)
    (setf xs (alloc-array 'int 100))
    (free-array xs)
    (is-error (array-size xs)
              simple-error
              "Array already freed.")))

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

(subtest "array-ensure-lisp-up-to-date"

  (with-cuda (0)
    (with-array (xs int 1)
      (array-ensure-lisp-up-to-date xs)
      (ok (avm.api.array::array-lisp-up-to-date-p xs)
          "Lisp memory is up-to-date.")))

  (with-cuda (0)
    (with-array (xs int 1)
      (array-set-cuda-dirty xs)
      (array-ensure-lisp-up-to-date xs)
      (ok (avm.api.array::array-lisp-up-to-date-p xs)
          "Lisp memory is not up-to-date, device memory is.")))

  (with-cuda (0)
    (is-error (array-ensure-lisp-up-to-date :foo)
              type-error
              "Invalid array."))

  (with-cuda (0)
    (with-array (xs int 1)
      (setf (avm.api.array::array-%lisp-up-to-date xs) nil)
      (setf (avm.api.array::array-%cuda-up-to-date xs) nil)
      (is-error (array-ensure-lisp-up-to-date xs)
                simple-error
                "Lisp memory is up-to-date, device memory is not.")))
  )

(subtest "array-ensure-cuda-up-to-date"

  (with-cuda (0)
    (with-array (xs int 1)
      (array-ensure-cuda-up-to-date xs)
      (ok (avm.api.array::array-%cuda-up-to-date xs)
          "Device memory is up-to-date.")))

  (with-cuda (0)
    (with-array (xs int 1)
      (array-set-lisp-dirty xs)
      (array-ensure-cuda-up-to-date xs)
      (ok (avm.api.array::array-%cuda-up-to-date xs)
          "Device memory is not up-to-date, Lisp memory is.")))

  (is-error (array-ensure-cuda-up-to-date :foo)
            type-error
            "Invalid array.")

  (with-cuda (0)
    (with-array (xs int 1)
      (setf (avm.api.array::array-%lisp-up-to-date xs) nil)
      (setf (avm.api.array::array-%cuda-up-to-date xs) nil)
      (is-error (array-ensure-cuda-up-to-date xs)
                simple-error
                "Device memory is not up-to-date, Lisp memory is not.")))
  )


(finalize)
