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
           :array-lisp
           :array-host
           :array-device
           :array-aref
           :array-size
           :set-array-dirty
           :array-ensure-lisp-up-to-date
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
;; Array

(defstruct (avm-array (:constructor %make-farray)
                      (:conc-name array-)
                      (:predicate array-p))
  (base-type :base-type :read-only t)
  (lisp :lisp :read-only t)
  (lisp-up-to-date :lisp-up-to-date)
  (host :host :read-only t)
  (host-up-to-date :host-up-to-date)
  (device :device :read-only t)
  (device-up-to-date :device-up-to-date))


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
                  
(defun alloc-array (type dimensions)
  (let ((cuda-type (lisp->cuda-type type)))
    (let ((lisp (make-tuple-array type dimensions))
          (host (and *use-cuda-p*
                     (cl-cuda:alloc-host-memory cuda-type dimensions)))
          (device (and *use-cuda-p*
                       (cl-cuda:alloc-device-memory cuda-type dimensions))))
      (%make-farray :base-type type
                    :lisp lisp :lisp-up-to-date t
                    :host host :host-up-to-date t
                    :device device :device-up-to-date t))))

(defun free-array (array)
  (when *use-cuda-p*
    (cl-cuda:free-device-memory (array-device array))
    (cl-cuda:free-host-memory (array-host array))))

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
  (array-ensure-lisp-up-to-date array)
  (let ((lisp (array-lisp array))
        (base-type (array-base-type array)))
    (tuple-aref lisp base-type index)))

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
          (array-ensure-lisp-up-to-date ,getter)
          (let ((lisp (array-lisp ,getter))
                (base-type (array-base-type ,getter)))
            (setf (tuple-aref lisp base-type ,temp-index)
                  (values ,v1 ,v2 ,v3 ,v4)))
          (set-array-dirty ,getter :lisp))
       `(tuple-aref ,getter (array-base-type ,getter) ,temp-index)))))

(defun array-size (array)
  (tuple-array-dimensions
   (array-lisp array)
   (array-base-type array)))

(defmethod sync-array (array (from (eql :host)) (to (eql :lisp)))
  (setf (array-lisp-up-to-date array) t)
  nil)

(defun set-array-dirty (array avm)
  (setf (array-lisp-up-to-date array) (eq avm :lisp))
  (setf (array-host-up-to-date array) (eq avm :host))
  (setf (array-device-up-to-date array) (eq avm :device)))

(defun array-ensure-lisp-up-to-date (array)
  (when (not (array-lisp-up-to-date array))
    (cond
      ((array-host-up-to-date array)
       (sync-array array :host :lisp))
      ((array-device-up-to-date array)
       (sync-array array :device :host)
       (sync-array array :host :lisp))
      (t (error "Must not be reached.")))))
