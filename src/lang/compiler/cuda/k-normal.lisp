#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.compiler.cuda.k-normal
  (:use :cl
        :avm
        :avm.lang.syntax)
  (:export :k-normal
           :*gentmp-counter*))
(in-package :avm.lang.compiler.cuda.k-normal)


;;
;; Gentmp

(defvar *gentmp-counter* 0)

(defun gentmp ()
  (prog1 (intern (format nil "T~A" *gentmp-counter*))
    (incf *gentmp-counter*)))


;;
;; K-normalization

(defun k-normal (form)
  (cond
    ((literal-p form) (k-normal-literal form))
    ((reference-p form) (k-normal-reference form))
    ((accessor-p form) (k-normal-accessor form))
    ((the-p form) (k-normal-the form))
    ((if-p form) (k-normal-if form))
    ((let-p form) (k-normal-let form))
    ((flet-p form) (k-normal-flet form))
    ((labels-p form) (k-normal-labels form))
    ((set-p form) (k-normal-set form))
    ((apply-p form) (k-normal-apply form))
    (t (error "The value ~S is an invalid form." form))))

(defun k-normal-literal (form)
  form)

(defun k-normal-reference (form)
  form)

(defun k-normal-accessor (form)
  (k-normal-apply form))

(defun k-normal-the (form)
  (let ((type (the-type form))
        (value (the-value form)))
    (if (or (literal-p value) (reference-p value))
        `(the ,type ,value)
        (let ((tmp (gentmp))
              (value1 (k-normal value)))
          `(let ((,tmp ,value1))
             (the ,type ,tmp))))))

(defun k-normal-if (form)
  (let ((test-form (if-test-form form))
        (then-form (if-then-form form))
        (else-form (if-else-form form)))
    (let ((tmp (gentmp))
          (test-form1 (k-normal test-form))
          (then-form1 (k-normal then-form))
          (else-form1 (k-normal else-form)))
      (if (or (literal-p test-form) (reference-p test-form))
          `(if ,test-form ,then-form1 ,else-form1)
          `(let ((,tmp ,test-form1))
             (if ,tmp ,then-form1 ,else-form1))))))

(defun k-normal-let (form)
  (flet ((aux (binding)
           (destructuring-bind (var value) binding
             (let ((value1 (k-normal value)))
               `(,var ,value1)))))
    (let ((bindings (let-bindings form))
          (body (let-body form)))
      (let ((bindings1 (mapcar #'aux bindings))
            (body1 (k-normal body)))
        `(let ,bindings1 ,body1)))))

(defun k-normal-flet (form)
  (let ((bindings (flet-bindings form))
        (body (flet-body form)))
    (%k-normal-flet 'flet bindings body)))

(defun %k-normal-flet (op bindings body)
  (flet ((aux (binding)
           (destructuring-bind (name args body) binding
             (let ((body1 (k-normal body)))
               `(,name ,args ,body1)))))
    (let ((bindings1 (mapcar #'aux bindings))
          (body1 (k-normal body)))
      `(,op ,bindings1 ,body1))))

(defun k-normal-labels (form)
  (let ((bindings (labels-bindings form))
        (body (labels-body form)))
    (%k-normal-flet 'labels bindings body)))

(defun k-normal-set (form)
  (let ((place (set-place form))
        (value (set-value form)))
    (multiple-value-bind (fn place1) (k-normal-place place)
      (if (or (literal-p value)
              (reference-p value))
          (funcall fn `(set ,place1 ,value))
          (let ((tmp (gentmp))
                (value1 (k-normal value)))
            `(let ((,tmp ,value1))
               ,(funcall fn `(set ,place1 ,tmp))))))))

(defun k-normal-place (place)
  (cond
    ((reference-place-p place) (k-normal-reference-place place))
    ((vector-place-p place) (k-normal-vector-place place))
    ((array-place-p place) (k-normal-array-place place))
    (t (error "Must not be reached."))))

(defun k-normal-reference-place (place)
  (values #'identity place))

(defun k-normal-vector-place (place)
  (let ((operator (vector-place-operator place))
        (value (vector-place-value place)))
    (multiple-value-bind (fn value1) (k-normal-place value)
      (values fn `(,operator ,value1)))))

(defun k-normal-array-place (place)
  (let ((value (array-place-value place))
        (index (array-place-index place)))
    (multiple-value-bind (fn value1) (k-normal-place value)
      (if (or (literal-p index)
              (reference-p index))
          (values fn `(aref ,value1 ,index))
          (let ((tmp (gentmp))
                (index1 (k-normal index)))
            (values #'(lambda (form)
                        `(let ((,tmp ,index1))
                           ,(funcall fn form)))
                    `(aref ,value1 ,tmp)))))))

(defun k-normal-apply (form)
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (%k-normal-apply operator operands nil)))

(defun %k-normal-apply (operator operands tmps)
  (if operands
      (destructuring-bind (operand . operands1) operands
        (if (or (literal-p operand) (reference-p operand))
            (%k-normal-apply operator operands1 (cons operand tmps))
            (let ((tmp (gentmp))
                  (operand1 (k-normal operand)))
              `(let ((,tmp ,operand1))
                 ,(%k-normal-apply operator operands1 (cons tmp tmps))))))
      `(,operator ,@(nreverse tmps))))
