#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo.api.kernel-manager
  (:use :cl
        :foo
        :foo.lang
        :foo.lang.type
        :foo.lang.kernel)
  (:export :make-kernel-manager
           :*kernel-manager*
           :kernel-manager-define-function
           ))
(in-package :foo.api.kernel-manager)


;;
;; Kernel manager

(defstruct (kernel-manager (:constructor %make-kernel-manager))
  (kernel :kernel :read-only t))

(defun make-kernel-manager ()
  (%make-kernel-manager :kernel (make-kernel)))

(defvar *kernel-manager* (make-kernel-manager))

(defgeneric kernel-manager-define-function (manager engine name args body))

(defmethod kernel-manager-define-function (manager (engine (eql :cl))
                                           name args body)
  ;; Check reserved arguments not used.
  (unless (not (member 'i args))
    (error "The argument I is reserved."))
  (unless (not (member 'n args))
    (error "The argument N is reserved."))
  ;; Compile and define kernel function.
  (let ((kernel (kernel-manager-kernel manager))
        (args1 (append '(i n) args)))
    ;; Compile kernel function.
    (multiple-value-bind (name1 type form)
        (compile-kernel-function :cl name args1 body kernel)
      ;; Define kernel function to kernel.
      (kernel-define-function kernel name1 type args1 body)
      ;; Return compiled form.
      (values name1 (include-vector-type-p type) form))))

(defun include-vector-type-p (type)
  (some #'vector-type-p (butlast type)))
