#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.appenv
  (:use :cl
        :avm.lang.symbol
        :avm.lang.type
        :avm.lang.unienv)
  (:export :empty-appenv
           :extend-appenv
           :query-appenv
           :subst-appenv))
(in-package :avm.lang.appenv)


;;
;; Function application environment

(defun empty-appenv ()
  nil)

(defun extend-appenv (form type aenv)
  (check-type type function-type)
  (acons form type aenv))

(defun query-appenv (form aenv)
  (or (cdr (assoc form aenv))
      (error "The function application ~S not found." form)))

(defun subst-appenv (uenv aenv)
  (loop for (form . ftype) in aenv
     collect
       (let ((ftype1 (loop for type in ftype
                        collect
                          (query-unienv type uenv))))
         (cons form ftype1))))
