#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.api.cuda
  (:use :cl
        :avm)
  (:export :with-cuda
           :*use-cuda-p*
           :synchronize))
(in-package :avm.api.cuda)


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

(defun synchronize ()
  (when cl-cuda:*cuda-context*
    (cl-cuda:synchronize-context)))
