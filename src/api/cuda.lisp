#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo.api.cuda
  (:use :cl
        :foo)
  (:export :with-cuda
           :*use-cuda-p*))
(in-package :foo.api.cuda)


;;
;; CUDA

(defvar *use-cuda-p* nil)

(defmacro with-cuda ((dev-id) &body body)
  ;; TODO: once only
  `(flet ((aux ()
            ,@body))
     (if ,dev-id
         (let ((*use-cuda-p* t))
           (cl-cuda:with-cuda (,dev-id)
             (aux)))
         (aux))))
