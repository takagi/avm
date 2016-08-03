#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.api.kernel-manager
  (:use :cl
        :avm
        :avm.lang
        :avm.lang.type
        :avm.lang.kernel)
  (:export :make-kernel-manager
           :*kernel-manager*
           :kernel-manager-define-function
           :kernel-manager-define-macro
           ))
(in-package :avm.api.kernel-manager)


;;
;; Kernel manager

(defstruct (kernel-manager (:constructor %make-kernel-manager))
  (kernel :kernel :read-only t))

(defun make-kernel-manager ()
  (%make-kernel-manager :kernel (make-kernel)))

(defvar *kernel-manager* (make-kernel-manager))

(defun kernel-manager-define-function (manager name args body)
  ;; Check reserved arguments not used.
  (unless (not (member 'i args))
    (error "The argument I is reserved."))
  (unless (not (member 'n args))
    (error "The argument N is reserved."))
  (let ((args1 (append '(i n) args)))
    ;; Compile and define kernel function.
    (let ((kernel (kernel-manager-kernel manager)))
      ;; Compile kernel function.
      (multiple-value-bind (lisp-name ftype lisp-form)
          (compile-kernel-function :lisp name args1 body kernel)
        (multiple-value-bind (cuda-name _ cuda-form)
            (compile-kernel-function :cuda name args1 body kernel)
          (declare (ignore _))
          ;; Define kernel function to kernel.
          (kernel-define-function kernel name lisp-name ftype args1 body)
          ;; Return compiled form.
          (values lisp-name lisp-form cuda-name cuda-form
                  (include-vector-type-p ftype)))))))

(defun include-vector-type-p (type)
  (some #'vector-type-p (function-arg-types type)))

(defun kernel-manager-define-macro (manager name args body)
  (let ((kernel (kernel-manager-kernel manager)))
    (kernel-define-macro kernel name args body)))
