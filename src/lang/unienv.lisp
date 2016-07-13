#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.unienv
  (:use :cl
        :avm.lang.type)
  (:import-from :cl-unification
                :make-empty-environment
                :find-variable-value)
  (:export :empty-unienv
           :query-unienv
           :unify))
(in-package :avm.lang.unienv)


;;
;; Unification environment

(defun empty-unienv ()
  (list nil nil (make-empty-environment)))

(defun query-unienv (type uenv)
  (check-type type avm-type)
  (cond
    ((scalar-type-p type) type)
    ((vector-type-p type)
     (let ((base-type (vector-type-base-type type))
           (size (vector-type-size type)))
       (let ((base-type1 (query-unienv base-type uenv))
             (size1 (%query-unienv size uenv)))
         `(:vector ,base-type1 ,size1))))
    ((array-type-p type)
     (let ((base-type (array-type-base-type type)))
       (let ((base-type1 (query-unienv base-type uenv)))
         `(:array ,base-type1))))
    ((type-variable-p type)
     (destructuring-bind (lhs rhs subst) uenv
       (declare (ignore lhs rhs))
       (let ((type1 (find-variable-value type subst)))
         (if type1
             (query-unienv type1 uenv)
             type))))
    (t (error "Must not be reached."))))

(defun %query-unienv (size uenv)
  (assert (avm.lang.type::vector-type-size-p size))
  (if (type-variable-p size)
      (destructuring-bind (lhs rhs subst) uenv
        (declare (ignore lhs rhs))
        (let ((size1 (find-variable-value size subst)))
          (if size1
              (%query-unienv size1 uenv)
              size)))
      size))

(defun unify (type1 type2 uenv)
  (check-type type1 avm-type)
  (check-type type2 avm-type)
  (destructuring-bind (lhs rhs subst) uenv
    (let* ((lhs1 (cons type1 lhs))
           (rhs1 (cons type2 rhs))
           (subst1 (cl-unification:unify lhs1 rhs1 subst)))
      (let* ((uenv1 (list lhs1 rhs1 subst1))
             (type (query-unienv type1 uenv1)))
        (values type uenv1)))))
