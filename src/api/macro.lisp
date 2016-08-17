#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.api.macro
  (:use :cl
        :avm
        :avm.api.kernel-manager)
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
