#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.api.array
  (:use :cl
        :avm
        :avm.lang.data)
  (:export :avm-array
           :array-p
           :alloc-array
           :free-array
           :with-array
           :with-arrays
           :array-aref
           :array-size
           :set-array-lisp-dirty
           :set-array-cuda-dirty
           :array-ensure-lisp-up-to-date
           :array-ensure-cuda-up-to-date
           ))
(in-package :avm.api.array)


;;
;; Tuple array helpers

(defun make-tuple-array (type dimensions)
  (ecase type
    (int (make-int-array dimensions))
    (int2 (make-int2-array dimensions))
    (int3 (make-int3-array dimensions))
    (int4 (make-int4-array dimensions))
    (float (make-float-array dimensions))
    (float2 (make-float2-array dimensions))
    (float3 (make-float3-array dimensions))
    (float4 (make-float4-array dimensions))
    (double (make-double-array dimensions))
    (double2 (make-double2-array dimensions))
    (double3 (make-double3-array dimensions))
    (double4 (make-double4-array dimensions))))

(defun tuple-array-dimensions (array base-type)
  (ecase base-type
    (int (int-array-dimensions array))
    (int2 (int2-array-dimensions array))
    (int3 (int3-array-dimensions array))
    (int4 (int4-array-dimensions array))
    (float (float-array-dimensions array))
    (float2 (float2-array-dimensions array))
    (float3 (float3-array-dimensions array))
    (float4 (float4-array-dimensions array))
    (double (double-array-dimensions array))
    (double2 (double2-array-dimensions array))
    (double3 (double3-array-dimensions array))
    (double4 (double4-array-dimensions array))))

(defun tuple-aref (array base-type index)
  (ecase base-type
    (int (int-aref* array index))
    (int2 (int2-aref* array index))
    (int3 (int3-aref* array index))
    (int4 (int4-aref* array index))
    (float (float-aref* array index))
    (float2 (float2-aref* array index))
    (float3 (float3-aref* array index))
    (float4 (float4-aref* array index))
    (double (double-aref* array index))
    (double2 (double2-aref* array index))
    (double3 (double3-aref* array index))
    (double4 (double4-aref* array index))))

(define-setf-expander tuple-aref (array base-type index &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion array env)
    (declare (ignore newval setter))
    (let ((v1 (gensym "X"))
          (v2 (gensym "Y"))
          (v3 (gensym "Z"))
          (v4 (gensym "W"))
          (temp-base-type (gensym))
          (temp-index (gensym)))
      (values
       `(,@dummies ,temp-base-type ,temp-index)
       `(,@vals ,base-type ,index)
       `(,v1 ,v2 ,v3 ,v4)
       `(ecase ,temp-base-type
          ((int int2 int3 int4)
           (check-type ,v1 (or null fixnum))
           (check-type ,v2 (or null fixnum))
           (check-type ,v3 (or null fixnum))
           (check-type ,v4 (or null fixnum))
           (ecase ,temp-base-type
             (int (setf (int-aref* ,getter ,temp-index) ,v1))
             (int2 (setf (int2-aref* ,getter ,temp-index) (values ,v1 ,v2)))
             (int3 (setf (int3-aref* ,getter ,temp-index) (values ,v1 ,v2 ,v3)))
             (int4 (setf (int4-aref* ,getter ,temp-index)
                         (values ,v1 ,v2 ,v3 ,v4)))))
          ((float float2 float3 float4)
           (check-type ,v1 (or null single-float))
           (check-type ,v2 (or null single-float))
           (check-type ,v3 (or null single-float))
           (check-type ,v4 (or null single-float))
           (ecase ,temp-base-type
             (float (setf (float-aref* ,getter ,temp-index) ,v1))
             (float2 (setf (float2-aref* ,getter ,temp-index)
                           (values ,v1 ,v2)))
             (float3 (setf (float3-aref* ,getter ,temp-index)
                           (values ,v1 ,v2 ,v3)))
             (float4 (setf (float4-aref* ,getter ,temp-index)
                           (values ,v1 ,v2 ,v3 ,v4)))))
          ((double double2 double3 double4)
           (check-type ,v1 (or null double-float))
           (check-type ,v2 (or null double-float))
           (check-type ,v3 (or null double-float))
           (check-type ,v4 (or null double-float))
           (ecase ,temp-base-type
             (double (setf (double-aref* ,getter ,temp-index) ,v1))
             (double2 (setf (double2-aref* ,getter ,temp-index)
                            (values ,v1 ,v2)))
             (double3 (setf (double3-aref* ,getter ,temp-index)
                            (values ,v1 ,v2 ,v3)))
             (double4 (setf (double4-aref* ,getter ,temp-index)
                            (values ,v1 ,v2 ,v3 ,v4))))))
       `(ecase ,temp-base-type
          (int (int-aref* ,getter ,temp-index))
          (int2 (int2-aref* ,getter ,temp-index))
          (int3 (int3-aref* ,getter ,temp-index))
          (int4 (int4-aref* ,getter ,temp-index))
          (float (float-aref* ,getter ,temp-index))
          (float2 (float2-aref* ,getter ,temp-index))
          (float3 (float3-aref* ,getter ,temp-index))
          (float4 (float4-aref* ,getter ,temp-index))
          (double (double-aref* ,getter ,temp-index))
          (double2 (double2-aref* ,getter ,temp-index))
          (double3 (double3-aref* ,getter ,temp-index))
          (double4 (double4-aref* ,getter ,temp-index))
          )))))


;;
;; Type conversion

(defparameter *lisp-cuda-types*
  '((int cl-cuda:int)
;    (int2 cl-cuda:int2)
;    (int3 cl-cuda:int3)
;    (int4 cl-cuda:int4)
    (float cl-cuda:float)
;    (float2 cl-cuda:float2)
    (float3 cl-cuda:float3)
    (float4 cl-cuda:float4)
    (double cl-cuda:double)
;    (double2 cl-cuda:double2)
    (double3 cl-cuda:double3)
    (double4 cl-cuda:double4)))

(defun lisp->cuda-type (lisp-type)
  (second (find lisp-type *lisp-cuda-types* :key #'first)))

(defun cuda->lisp-type (cuda-type)
  (first (find cuda-type *lisp-cuda-types* :key #'second)))


;;
;; Array

(defstruct (avm-array (:constructor %make-array)
                      (:conc-name array-)
                      (:predicate array-p))
  (base-type :base-type :read-only t)
  (tuple-array :tuple-array :read-only t)
  (host-ptr :host-ptr)
  (device-ptr :device-ptr)
  (lisp-up-to-date :lisp-up-to-date)
  (cuda-up-to-date :cuda-up-to-date))

(defun alloc-array (type dimensions)
  (let ((tuple-array (make-tuple-array type dimensions)))
    (let* ((cuda-type (lisp->cuda-type type))
           (host-ptr (and *use-cuda-p*
                      (cl-cuda:alloc-host-memory cuda-type dimensions)))
           (device-ptr (and *use-cuda-p*
                        (cl-cuda:alloc-device-memory cuda-type dimensions))))
      (%make-array :base-type type
                   :tuple-array tuple-array
                   :host-ptr host-ptr
                   :device-ptr device-ptr
                   :lisp-up-to-date t
                   :cuda-up-to-date t))))

(defun free-array (array)
  ;; Free device memory.
  (when (and *use-cuda-p*
             (array-device-ptr array))
    (cl-cuda:free-device-memory (array-device-ptr array))
    (setf (array-device-ptr array) nil))
  ;; Free host memory.
  (when (and *use-cuda-p*
             (array-host-ptr array))
    (cl-cuda:free-host-memory (array-host-ptr array))
    (setf (array-host-ptr array) nil)))

(defmacro with-array ((var type dimensions) &body body)
  `(let ((,var (alloc-array ',type ,dimensions)))
     (unwind-protect (progn ,@body)
       (free-array ,var))))

(defmacro with-arrays (bindings &body body)
  (if bindings
      (destructuring-bind (binding . rest) bindings
        `(with-array ,binding
           (with-arrays ,rest ,@body)))
      `(progn ,@body)))

(defun array-aref (array index)
  ;; Ensure array up-to-date for lisp.
  (array-ensure-lisp-up-to-date array)
  ;; Return element of tuple array.
  (let ((tuple-array (array-tuple-array array))
        (base-type (array-base-type array)))
    (tuple-aref tuple-array base-type index)))

(define-setf-expander array-aref (array index &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion array env)
    (declare (ignore newval setter))
    (let ((v1 (gensym "X"))
          (v2 (gensym "Y"))
          (v3 (gensym "Z"))
          (v4 (gensym "W"))
          (temp-index (gensym)))
      (values
       `(,@dummies ,temp-index)
       `(,@vals ,index)
       `(,v1 ,v2 ,v3 ,v4)
       `(progn
          ;; Ensure array up to date for lisp.
          (array-ensure-lisp-up-to-date ,getter)
          ;; Set element of tuple array.
          (let ((tuple-array (array-tuple-array ,getter))
                (base-type (array-base-type ,getter)))
            (setf (tuple-aref tuple-array base-type ,temp-index)
                  (values ,v1 ,v2 ,v3 ,v4)))
          ;; Set dirty flag for lisp to array.
          (set-array-lisp-dirty ,getter))
       ;; Return element of tuple array.
       `(let ((tuple-array (array-tuple-array ,getter))
              (base-type (array-base-type ,getter)))
          (tuple-aref tuple-array base-type ,temp-index))))))

(defun array-size (array)
  (let ((tuple-array (array-tuple-array array))
        (base-type (array-base-type array)))
    (tuple-array-dimensions tuple-array base-type)))

(defgeneric sync-array (array from to))

(defmethod sync-array (array (from (eql :lisp)) (to (eql :cuda)))
  (error "Not implemented."))

(defmethod sync-array (array (from (eql :cuda)) (to (eql :lisp)))
  ;; Synchronize device to host.
  (let ((host-ptr (array-host-ptr array))
        (device-ptr (array-device-ptr array))
        (cuda-type (lisp->cuda-type (array-base-type array)))
        (size (array-size array)))
    (cl-cuda::memcpy-device-to-host host-ptr device-ptr cuda-type size))
  ;; Synchronize host memory to tuple array.
  (let* ((tuple-array (array-tuple-array array))
         (host-ptr (array-host-ptr array))
         (base-type (array-base-type array))
         (cuda-type (lisp->cuda-type base-type))
         (size (array-size array)))
    (dotimes (i size)
      ;; TODO: Not work with vector types.
      (setf (tuple-aref tuple-array base-type i)
            (cl-cuda:host-memory-aref host-ptr cuda-type i)))))

(defun set-array-lisp-dirty (array)
  (unless (array-lisp-up-to-date array)
    (error "Ensure array up-to-date for lisp first."))
  (setf (array-cuda-up-to-date array) nil))

(defun set-array-cuda-dirty (array)
  (unless (array-cuda-up-to-date array)
    (error "Ensure array up-to-date for CUDA first."))
  (setf (array-lisp-up-to-date array) nil))

(defun array-ensure-lisp-up-to-date (array)
  (when (not (array-lisp-up-to-date array))
    (assert (array-cuda-up-to-date array))
    (sync-array array :cuda :lisp)))

(defun array-ensure-cuda-up-to-date (array)
  (when (not (array-cuda-up-to-date array))
    (assert (array-lisp-up-to-date array))
    (sync-array array :lisp :cuda)))
