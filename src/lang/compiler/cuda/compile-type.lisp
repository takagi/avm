#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.compiler.cuda.compile-type
  (:use :cl
        :avm)
  (:export :compile-type))
(in-package :avm.lang.compiler.cuda.compile-type)


(defun compile-type (type)
  ;; TODO: use abstract type
  (cl-pattern:match type
    ('bool 'cl-cuda:bool)
    ('int 'cl-cuda:int)
    ('float 'cl-cuda:float)
    ('double 'cl-cuda:double)
    ((:vector base-type size)
     (intern (format nil "~A~A" base-type size) (find-package :cl-cuda)))
    ((:array type1)
     (cl-pattern:match type1
       ('int 'cl-cuda:int*)
       ('float 'cl-cuda:float*)
       ('double 'cl-cuda:double*)
       ((:vector base-type size)
        (intern (format nil "~A~A*" base-type size) (find-package :cl-cuda)))
       (_ (error "Must not be reached."))))
    (_ (error "Must not be reached."))))
