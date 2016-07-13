#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.typenv
  (:use :cl
        :avm.lang.symbol
        :avm.lang.type
        :avm.lang.unienv)
  (:export :empty-typenv
           :extend-typenv
           :query-typenv
           :subst-typenv))
(in-package :avm.lang.typenv)


;;
;; Type environment

(defun empty-typenv ()
  nil)

(defun extend-typenv (var type tenv)
  (check-type var avm-symbol)
  (check-type type avm-type)
  (acons var type tenv))

(defun query-typenv (var tenv)
  (or (cdr (assoc var tenv))
      (error "The variable ~S not found." var)))

(defun subst-typenv (uenv tenv)
  (loop for (var . type) in tenv
     collect
       (let ((type1 (query-unienv type uenv)))
         (cons var type1))))
