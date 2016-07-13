#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.symbol
  (:use :cl)
  (:export :avm-symbol
           :avm-symbol-p
           ))
(in-package :avm.lang.symbol)


;;
;; Symbol

(deftype avm-symbol ()
  '(satisfies avm-symbol-p))

(defun avm-symbol-p (object)
  (symbolp object))
