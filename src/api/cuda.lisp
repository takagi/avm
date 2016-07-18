#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.api.cuda
  (:use :cl
        :avm)
  (:import-from :alexandria
                :once-only)
  (:export :with-cuda
           :*use-cuda-p*
           :cuda-state-not-available-p
           :cuda-state-available-p
           :cuda-state-used-p
           :cuda-available-p
           :synchronize))
(in-package :avm.api.cuda)


;;
;; CUDA

(defvar *use-cuda-p* nil)

(defmacro with-cuda ((dev-id) &body body)
  (once-only (dev-id)
    `(flet ((aux () ,@body))
       (if (and (not cl-cuda:*sdk-not-found*)
                ,dev-id)
           (let ((*use-cuda-p* t))
             (cl-cuda:with-cuda (,dev-id)
               (aux)))
           (aux)))))

(defun cuda-state-not-available-p ()
  (not cl-cuda:*cuda-context*))

(defun cuda-state-available-p ()
  (and cl-cuda:*cuda-context*
       (not *use-cuda-p*)))

(defun cuda-state-used-p ()
  (and cl-cuda:*cuda-context*
       *use-cuda-p*
       t))

(defun cuda-available-p ()
  ;; Equivalent to CUDA state Available or Used.
  (and cl-cuda:*cuda-context*
       t))

(defun synchronize ()
  (when (cuda-available-p)
    (cl-cuda:synchronize-context)))
