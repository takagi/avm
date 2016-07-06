#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo.lang.symbol
  (:use :cl)
  (:export :foo-symbol
           :foo-symbol-p
           ))
(in-package :foo.lang.symbol)


;;
;; Symbol

(deftype foo-symbol ()
  '(satisfies foo-symbol-p))

(defun foo-symbol-p (object)
  (symbolp object))
