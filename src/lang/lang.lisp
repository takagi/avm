#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang
  (:use :cl)
  (:export :compile-kernel-function
           :compile-kernel-global
           :compile-kernel-constant
           ))
(in-package :avm.lang)


;;
;; Compiler top

(defgeneric compile-kernel-function (engine name args body kernel))

(defgeneric compile-kernel-global (kernel name engine))

(defgeneric compile-kernel-constant (kernel name engine))
