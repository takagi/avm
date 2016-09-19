#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.api.macro
  (:use :cl
        :avm
        :avm.api.kernel-manager)
  (:import-from :alexandria
                :with-gensyms)
  (:export :expand-macro-1
           :expand-macro
           ))
(in-package :avm.api.macro)


;;
;; EXPAND-MACRO

(defun expand-macro-1 (form &optional (manager *kernel-manager*))
  (kernel-manager-expand-macro-1 manager form))

(defun expand-macro (form &optional (manager *kernel-manager*))
  (kernel-manager-expand-macro manager form))


;;
;; PROGN

(defun progn-form (body)
  (if (and (listp body)
           (not (null body)))
      (destructuring-bind (form . body1) body
        (if body1
            (with-gensyms (var)
              `(let ((,var ,form))
                 (progn ,@body1)))
            form))
      (error "The value ~S is an invalid form." `(progn ,@body))))

(defkernel-macro progn (&body body)
  (progn-form body))


;;
;; LET*

(defun let*-form (bindings body)
  (if bindings
      (destructuring-bind (binding . bindings1) bindings
        `(let (,binding)
           (let* ,bindings1
             ,@body)))
      `(progn ,@body)))

(defkernel-macro let* (bindings &body body)
  (let*-form bindings body))
