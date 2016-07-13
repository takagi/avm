#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo.lang.type
  (:use :cl
        :foo
        :foo.lang.symbol)
  (:export ;; Type
           :foo-type
           :foo-type-p
           :scalar-type-p
           :vector-type-p
           :vector-type-base-type
           :vector-type-size
           :array-type-p
           :array-type-base-type
           :type-variable-p
           :type-wildcard-p
           ;; Type of functions
           :function-type
           :function-type-p
           :make-function-type
           :function-arg-types
           :function-return-type
           ;; Parse
           :parse-type
           :unparse-type
           ;; Gentype
           :gentype
           :*gentype-counter*
           ;; Type scheme
           :type-scheme
           :type-scheme-p
           ;; Type scheme to type
           :type-scheme-to-type
           ))
(in-package :foo.lang.type)


;;
;; Type

(deftype foo-type ()
  '(satisfies foo-type-p))

(defun foo-type-p (object)
  (or (scalar-type-p object)
      (vector-type-p object)
      (array-type-p object)
      (type-variable-p object)))

(defun scalar-type-p (object)
  (and (member object '(bool int float double))
       t))

(defun vector-type-p (object)
  (cl-pattern:match object
    ((:vector object1 size)
     (and (or (scalar-type-p object1)
              (type-variable-p object1))
          (vector-type-size-p size)))
    (_ nil)))

(defun vector-type-size-p (object)
  (or (type-variable-p object)
      (type-wildcard-p object)
      (and (integerp object)
           (<= 2 object 4))))

(defun vector-type-base-type (type)
  (assert (vector-type-p type))
  (cl-pattern:match type
    ((:vector base-type _) base-type)))

(defun vector-type-size (type)
  (assert (vector-type-p type))
  (cl-pattern:match type
    ((:vector _ size) size)))

(defun array-type-p (object)
  (cl-pattern:match object
    ((:array base-type) (or (scalar-type-p base-type)
                            (vector-type-p base-type)
                            (type-variable-p base-type)))
    (_ nil)))

(defun array-type-base-type (type)
  (assert (array-type-p type))
  (cl-pattern:match type
    ((:array base-type) base-type)))

(defun type-variable-p (object)
  (and (cl-unification:variablep object)
       (not (type-wildcard-p object))))

(defun type-wildcard-p (object)
  (and (foo-symbol-p object)
       (string= object "_")))


;;
;; Type of functions

(deftype function-type ()
  '(satisfies function-type-p))

(defun function-type-p (type)
  (every #'foo-type-p type))

(defun make-function-type (arg-types return-type)
  (dolist (arg-type arg-types)
    (check-type arg-type foo-type))
  (check-type return-type foo-type)
  (append arg-types (list return-type)))

(defun function-arg-types (function-type)
  (butlast function-type))

(defun function-return-type (function-type)
  (car (last function-type)))


;;
;; Parse type

(defun parse-type (type)
  (ecase type
    (int 'int)
    (int2 '(:vector int 2))
    (int3 '(:vector int 3))
    (int4 '(:vector int 4))
    (float 'float)
    (float2 '(:vector float 2))
    (float3 '(:vector float 3))
    (float4 '(:vector float 4))
    (double 'double)
    (double2 '(:vector double 2))
    (double3 '(:vector double 3))
    (double4 '(:vector double 4))))

(defun unparse-type (type)
  (cl-pattern:match type
    ('int 'int)
    ('float 'float)
    ('double 'double)
    ((:vector 'int 2) 'int2)
    ((:vector 'int 3) 'int3)
    ((:vector 'int 4) 'int4)
    ((:vector 'float 2) 'float2)
    ((:vector 'float 3) 'float3)
    ((:vector 'float 4) 'float4)
    ((:vector 'double 2) 'double2)
    ((:vector 'double 3) 'double3)
    ((:vector 'double 4) 'double4)
    ((:array _) (error "Not implemented."))
    (_ (error "The value ~S is an invalid type." type))))


;;
;; Gentype

(defvar *gentype-counter* 0)

(defun gentype ()
  (prog1 (intern (format nil "?T~A" *gentype-counter*))
    (incf *gentype-counter*)))


;;
;; Type scheme

(deftype type-scheme ()
  '(satisfies type-scheme-p))

(defun type-scheme-p (object)
  (cl-pattern:match object
    ((:type-scheme . objects) (every #'type-symbol-p objects))
    (_ nil)))

(defun type-symbol-p (object)
  (or (scalar-type-symbol-p object)
      (vector-type-symbol-p object)
      (array-type-symbol-p object)))

(defun scalar-type-symbol-p (object)
  (or (scalar-type-p object)
      (type-scheme-variable-p object)))

(defun vector-type-symbol-p (object)
  (cl-pattern:match object
    ((:vector base-type size)
     (and (scalar-type-symbol-p base-type)
          (vector-type-symbol-size-p size)))
    (_ nil)))

(defun vector-type-symbol-size-p (object)
  (or (type-scheme-variable-p object)
      (type-wildcard-p object)
      (and (integerp object)
           (<= 2 object 4))))

(defun array-type-symbol-p (object)
  (cl-pattern:match object
    ((:array base-type)
     (or (scalar-type-symbol-p base-type)
         (vector-type-symbol-p base-type)))
    (_ nil)))

(defun type-scheme-variable-p (object)
  (and (foo-symbol-p object)
       (not (type-variable-p object))
       (not (type-wildcard-p object))))


;;
;; Type scheme to type

(defun type-scheme-to-type (type-scheme)
  (flet ((aux (types-dict type-symbol)
           (destructuring-bind (types dict) types-dict
             (multiple-value-bind (type dict1)
                 (type-symbol-to-type type-symbol dict)
               (let ((types1 (cons type types)))
                 (list types1 dict1))))))
    (check-type type-scheme type-scheme)
    (cl-pattern:match type-scheme
      ((:type-scheme . type-symbols)
       (nreverse
        (car
         (reduce #'aux type-symbols :initial-value '(nil nil))))))))

(defun type-symbol-to-type (type-symbol dict)
  (cond
    ((scalar-type-symbol-p type-symbol)
     (scalar-type-symbol-to-type type-symbol dict))
    ((vector-type-symbol-p type-symbol)
     (vector-type-symbol-to-type type-symbol dict))
    ((array-type-symbol-p type-symbol)
     (array-type-symbol-to-type type-symbol dict))
    (t (error "Must not be reached."))))

(defun scalar-type-symbol-to-type (type-symbol dict)
  (if (scalar-type-p type-symbol)
      (values type-symbol dict)
      (let ((type-symbol1 (cdr (assoc type-symbol dict))))
        (if type-symbol1
            (values type-symbol1 dict)
            (let* ((type-symbol2 (gentype))
                   (dict1 (acons type-symbol type-symbol2 dict)))
              (values type-symbol2 dict1))))))

(defun vector-type-symbol-to-type (type-symbol dict)
  (cl-pattern:match type-symbol
    ((:vector base-type size)
     (multiple-value-bind (base-type1 dict1)
         (scalar-type-symbol-to-type base-type dict)
       (multiple-value-bind (size1 dict2)
           (vector-type-symbol-size-to-type size dict1)
         (values `(:vector ,base-type1 ,size1) dict2))))))

(defun vector-type-symbol-size-to-type (size dict)
  (cond
    ((type-scheme-variable-p size)
     (let ((size1 (cdr (assoc size dict))))
       (if size1
           (values size1 dict)
           (let* ((size2 (gentype))
                  (dict1 (acons size size2 dict)))
             (values size2 dict1)))))
    ((type-wildcard-p size) (values size dict))
    ((integerp size) (values size dict))
    (t (error "Must not be reached."))))

(defun array-type-symbol-to-type (type-symbol dict)
  (cl-pattern:match type-symbol
    ((:array base-type)
     (multiple-value-bind (base-type1 dict1)
         (type-symbol-to-type base-type dict)
       (values `(:array ,base-type1) dict1)))))
