#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo.lang.data
  (:use :cl
        :cl-tuples
        :foo)
  (:export))                       ; Symbols are exported via EXPORTs.
(in-package :foo.lang.data)


;;
;; Helpers

(eval-when (:compile-toplevel)

  (defun type-base-type (type)
    (ecase type
      ((int int2 int3 int4) 'fixnum)
      ((float float2 float3 float4) 'single-float)
      ((double double2 double3 double4) 'double-float)))

  (defun type-size (type)
    (ecase type
      ((int2 float2 double2) 2)
      ((int3 float3 double3) 3)
      ((int4 float4 double4) 4)))

  (defun type-initial-element (type)
    (ecase (type-base-type type)
      (fixnum 0)
      (single-float 0.0f0)
      (double-float 0.0d0)))

  (defun type-elements (type)
    (subseq '(x y z w) 0 (type-size type)))

  (defun type-foo (type foo)
    (intern (format nil "~A-~A" type foo) #.*package*))

  (defun type-values* (type)
    (type-foo type "VALUES*"))

  (defun type-x* (type x)
    (type-foo type (format nil "~A*" x)))

  (defun type-aref* (type)
    (type-foo type "AREF*"))

  (defun type-make-array (type)
    (intern (format nil "MAKE-~A-ARRAY" type) #.*package*))

  (defun type-array (type)
    (intern (format nil "~A-ARRAY" type) #.*package*))
  
  (defun type-array-dimensions (type)
    (intern (format nil "~A-ARRAY-DIMENSIONS" type) #.*package*))

  (defun type-add* (type)
    (type-foo type "ADD*"))

  (defun type-sub* (type)
    (type-foo type "SUB*"))

  (defun type-scale* (type)
    (type-foo type "SCALE*"))

  (defun type-%scale* (type)
    (type-foo type "%SCALE*"))

  (defun type-scale-recip* (type)
    (type-foo type "SCALE-RECIP*"))

  (defun type-norm* (type)
    (type-foo type "NORM*"))

  (defun type-args (type i)
    (loop for arg in (type-elements type)
       collect (intern (format nil "~A~A" arg i) #.*package*)))

  (defun type-xs (type x)
    (loop repeat (type-size type)
       for i from 1
       collect (intern (format nil "~A~A" x i) #.*package*)))
  )


;;
;; Scalar types

(eval-when (:compile-toplevel)
  (defun define-scalar-type-form (type)
    `(progn
       ;; Define and export FOO type.
       ;(deftype ,type ()
       ;  ',(type-base-type type))
       ;(export ',type)
       ;; Define and export FOO-ARRAY type.
       (deftype ,(type-array type) ()
         '(simple-array ,(type-base-type type) *))
       (export ',(type-array type))
       ;; Define and export MAKE-FOO-ARRAY.
       (defun ,(type-make-array type) (dimensions &key initial-element)
         (make-array dimensions
                     :initial-element (or initial-element
                                          ,(type-initial-element type))
                     :element-type ',(type-base-type type)))
       (export ',(type-make-array type))
       ;; Define and export FOO-AREF*
       (defmacro ,(type-aref* type) (array array-index)
         `(aref ,array ,array-index))
       (export ',(type-aref* type))
       ;; Define and export FOO-ARRAY-DIMENSIONS.
       (defun ,(type-array-dimensions type) (array)
         (array-dimension array 0))
       (export ',(type-array-dimensions type))
       (declaim (ftype (function (,(type-array type)) fixnum)
                       ,(type-array-dimensions type)))
       )))

(defmacro define-scalar-type (type)
  (define-scalar-type-form type))

(define-scalar-type int)
(define-scalar-type float)
(define-scalar-type double)


;;
;; Vector types

(eval-when (:compile-toplevel)
  (defun define-tuple-type-form (type)
    `(progn
       ;; Define FOO tuple type.
       (def-tuple-type ,type
           :tuple-element-type ,(type-base-type type)
           :initial-element ,(type-initial-element type)
           :elements ,(type-elements type))
       ;; Export FOO type.
       ; (export ',type)
       ;; Export FOO-VALUES*.
       (export ',(type-values* type))
       ;; Define and export FOO-{x,y,z,w}*
       ,@(loop for x in (type-elements type)
            append
              `((def-tuple-op ,(type-x* type x)
                    ((a ,type ,(type-elements type)))
                  (:return ,(type-base-type type)
                           ,x))
                (export ',(type-x* type x))))
       ;; Define and export FOO-ARRAY type.
       (deftype ,(type-array type) ()
         '(simple-array ,(type-base-type type) *))
       (export ',(type-array type))
       ;; Export MAKE-FOO-ARRAY.
       (export ',(type-make-array type))
       ;; Export FOO-AREF*.
       (export ',(type-aref* type))
       ;; Export FOO-ARRAY-DIMENSIONS
       (export ',(type-array-dimensions type))
       (declaim (ftype (function (,(type-array type)) fixnum)
                       ,(type-array-dimensions type)))
       )))

(eval-when (:compile-toplevel)
  (defun define-tuple-arithmetic-form (type)
    `(progn
       ;; Define and export FOO-ADD* operator.
       (def-tuple-op ,(type-add* type)
           ((a ,type ,(type-args type 1))
            (b ,type ,(type-args type 2)))
         (:return ,type
                  (,(type-values* type) ,@(loop for x in (type-args type 1)
                                                for y in (type-args type 2)
                                             collect `(+ ,x ,y)))))
       (export ',(type-add* type))
       ;; Define and export FOO-SUB* operator.
       (def-tuple-op ,(type-sub* type)
           ((a ,type ,(type-args type 1))
            (b ,type ,(type-args type 2)))
         (:return ,type
                  (,(type-values* type) ,@(loop for x in (type-args type 1)
                                                for y in (type-args type 2)
                                             collect `(- ,x ,y)))))
       (export ',(type-sub* type))
       ;; Define and export FOO-SCALE* operator.
       (def-tuple-op ,(type-scale* type)
           ((a ,type ,(type-elements type))
            (val ,(type-base-type type)))
         (:return ,type
                  (let ((k val))
                    (,(type-values* type) ,@(loop for x in (type-elements type)
                                               collect `(* ,x k))))))
       (export ',(type-scale* type))
       ;; Define and export FOO-%SCALE* operator.
       (defmacro ,(type-%scale* type) (k x)
         (list ',(type-scale* type) x k))
       (export ',(type-%scale* type))
       ;; Define and export FOO-SCALE-RECIP* operator.
       (def-tuple-op ,(type-scale-recip* type)
           ((a ,type ,(type-elements type))
            (val ,(type-base-type type)))
         (:return ,type
                  (let ((k val))
                    (,(type-values* type) ,@(loop for x in (type-elements type)
                                               collect `(/ ,x k))))))
       (export ',(type-scale-recip* type))
       )))

(eval-when (:compile-toplevel)
  (defun define-tuple-math-form (type)
    `(progn
       ;; Define and export FOO-NORM* function.
       (def-tuple-op ,(type-norm* type)
           ((a ,type ,(type-elements type)))
         (:return ,(type-base-type type)
                  (sqrt (+ ,@(loop for x in (type-elements type)
                                collect `(* ,x ,x))))))
       (export ',(type-norm* type))
       )))

(eval-when (:compile-toplevel)
  (defun define-vector-type-form (type)
    `(progn
       ,@(cdr (define-tuple-type-form type))
       ,@(cdr (define-tuple-arithmetic-form type))
       ,@(cdr (define-tuple-math-form type))
       )))

(defmacro define-vector-type (type)
  (define-vector-type-form type))

(define-vector-type int2)
(define-vector-type int3)
(define-vector-type int4)
(define-vector-type float2)
(define-vector-type float3)
(define-vector-type float4)
(define-vector-type double2)
(define-vector-type double3)
(define-vector-type double4)
