#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.syntax
  (:use :cl
        :avm
        :avm.lang.symbol
        :avm.lang.type)
  (:export ;; Literal
           :literal-p
           :int-literal-p
           :float-literal-p
           :double-literal-p
           ;; Reference
           :reference-p
           ;; Accessor
           :accessor-p
           :vector-accessor-p
           :array-accessor-p
           ;; THE
           :the-p
           :the-type
           :the-value
           ;; IF
           :if-p
           :if-test-form
           :if-then-form
           :if-else-form
           ;; LET
           :let-p
           :let-bindings
           :let-body
           ;; FLET
           :flet-p
           :flet-bindings
           :flet-body
           ;; LABELS
           :labels-p
           :labels-bindings
           :labels-body
           ;; SETF
           :setf-p
           :setf-place
           :setf-value
           ;; Place
           :place-p
           :reference-place-p
           :vector-place-p
           :vector-place-operator
           :vector-place-value
           :array-place-p
           :array-place-value
           :array-place-index
           ;; Application
           :apply-p
           :apply-operator
           :apply-operands
           ))
(in-package :avm.lang.syntax)


;;
;; Literal

(defun literal-p (object)
  (or (int-literal-p object)
      (float-literal-p object)
      (double-literal-p object)))

(defun int-literal-p (object)
  (typep object 'fixnum))

(defun float-literal-p (object)
  (typep object 'single-float))

(defun double-literal-p (object)
  (typep object 'double-float))


;;
;; Reference

(defun reference-p (object)
  (avm-symbol-p object))


;;
;; Accessor

(defun accessor-p (object)
  (or (vector-accessor-p object)
      (array-accessor-p object)))

(defun vector-accessor-p (object)
  (cl-pattern:match object
    ((op . _)
     (member op
      '(int2-x int2-y
        int3-x int3-y int3-z
        int4-x int4-y int4-z int4-w
        float2-x float2-y
        float3-x float3-y float3-z
        float4-x float4-y float4-z float4-w
        double2-x double2-y
        double3-x double3-y double3-z
        double4-x double4-y double4-z double4-w)))
    (_ nil)))

(defun array-accessor-p (object)
  (cl-pattern:match object
    (('aref . _) t)
    (_ nil)))


;;
;; THE

(defun the-p (object)
  (cl-pattern:match object
    (('the . _) t)
    (_ nil)))

(defun the-type (form)
  (cl-pattern:match form
    (('the type _) type)
    (_ (error "The form ~S is malformed." form))))

(defun the-value (form)
  (cl-pattern:match form
    (('the _ value) value)
    (_ (error "The form ~S is malformed." form))))


;;
;; IF

(defun if-p (object)
  (cl-pattern:match object
    (('if . _) t)
    (_ nil)))

(defun if-test-form (form)
  (cl-pattern:match form
    (('if test-form _ _) test-form)
    (_ (error "The form ~S is malformed." form))))

(defun if-then-form (form)
  (cl-pattern:match form
    (('if _ then-form _) then-form)
    (_ (error "The form ~S is malformed." form))))

(defun if-else-form (form)
  (cl-pattern:match form
    (('if _ _ else-form) else-form)
    (_ (error "The form ~S is malformed." form))))


;;
;; LET

(defun let-p (object)
  (cl-pattern:match object
    (('let . _) t)
    (_ nil)))

(defun let-bindings (form)
  (cl-pattern:match form
    (('let bindings _)
     (unless (every #'binding-p bindings)
       (error "The form ~S is malformed." form))
     (unless (null #1=(find-duplicate (mapcar #'car bindings)))
       (error "The variable ~A occurs more than once in the LET." #1#))
     bindings)
    (_ (error "The form ~S is malformed." form))))

(defun binding-p (object)
  (cl-pattern:match object
    ((var _) (avm-symbol-p var))
    (_ nil)))

(defun find-duplicate (xs)
  (cond
    ((null xs) nil)
    ((member (car xs) (cdr xs)) (car xs))
    (t (find-duplicate (cdr xs)))))

(defun let-body (form)
  ;; TODO: implicit progn.
  (cl-pattern:match form
    (('let _ body) body)
    (_ (error "The form ~S is malformed." form))))


;;
;; FLET

(defun flet-p (object)
  (cl-pattern:match object
    (('flet . _) t)
    (_ nil)))

(defun flet-bindings (form)
  (cl-pattern:match form
    (('flet bindings _)
     (unless (every #'fbinding-p bindings)
       (error "The form ~S is malformed." form))
     (unless (not #1=(find-duplicate (mapcar #'car bindings)))
       (error "The function ~A occurs more than once in the FLET." #1#))
     bindings)
    (_ (error "The form ~S is malformed." form))))

(defun fbinding-p (object)
  (cl-pattern:match object
    ((name args _) (and (avm-symbol-p name)
                        (every #'avm-symbol-p args)))
    (_ nil)))

(defun flet-body (form)
  ;; TODO: implicit progn.
  (cl-pattern:match form
    (('flet _ body) body)
    (_ (error "The form ~S is malformed." form))))


;;
;; LABELS

(defun labels-p (object)
  (cl-pattern:match object
    (('labels . _) t)
    (_ nil)))

(defun labels-bindings (form)
  (cl-pattern:match form
    (('labels bindings _)
     (unless (every #'fbinding-p bindings)
       (error "The form ~S is malformed." form))
     (unless (not #1=(find-duplicate (mapcar #'car bindings)))
       (error "The function ~A occurs more than once in the LABELS." #1#))
     bindings)
    (_ (error "The form ~S is malformed." form))))

(defun labels-body (form)
  ;; TODO: implicit progn.
  (cl-pattern:match form
    (('labels _ body) body)
    (_ (error "The form ~S is malformed." form))))


;;
;; SETF

(defun setf-p (object)
  (cl-pattern:match object
    (('setf . _) t)
    (_ nil)))

(defun setf-place (form)
  (cl-pattern:match form
    (('setf place _)
     (unless (place-p place)
       (error "The form ~S is malformed." form))
     place)
    (_ (error "The form ~S is malformed." form))))

(defun setf-value (form)
  (cl-pattern:match form
    (('setf _ value) value)
    (_ (error "The form ~S is malformed." form))))


;;
;; Place

(defun place-p (object)
  (or (reference-place-p object)
      (vector-place-p object)
      (array-place-p object)))

(defun reference-place-p (object)
  (reference-p object))

(defun vector-place-p (object)
  (cl-pattern:match object
    ((op . _)
     (member op
      '(int2-x int2-y
        int3-x int3-y int3-z
        int4-x int4-y int4-z int4-w
        float2-x float2-y
        float3-x float3-y float3-z
        float4-x float4-y float4-z float4-w
        double2-x double2-y
        double3-x double3-y double3-z
        double4-x double4-y double4-z double4-w)))
    (_ nil)))

(defun vector-place-operator (place)
  (cl-pattern:match place
    ((operator _) operator)
    (_ (error "The form ~S is malformed." place))))

(defun vector-place-value (place)
  (cl-pattern:match place
    ((_ value)
     (unless (place-p value)
       (error "The form ~S is malformed." value))
     value)
    (_ (error "The form ~S is malformed." place))))

(defun array-place-p (object)
  (cl-pattern:match object
    (('aref . _) t)
    (_ nil)))

(defun array-place-value (place)
  (cl-pattern:match place
    (('aref value _)
     (unless (place-p value)
       (error "The form ~S is malformed." value))
     value)
    (_ (error "The form ~S is malformed." place))))

(defun array-place-index (place)
  (cl-pattern:match place
    (('aref _ index) index)
    (_ (error "The form ~S is malformed." place))))


;;
;; Application

(defun apply-p (object)
  (consp object))

(defun apply-operator (form)
  (car form))

(defun apply-operands (form)
  (cdr form))
