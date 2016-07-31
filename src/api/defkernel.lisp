#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.api.defkernel
  (:use :cl
        :avm
        :avm.api.cuda
        :avm.api.array
        :avm.api.kernel-manager)
  (:export :defkernel
           :*number-of-threads*
           :*compile-on-runtime*))
(in-package :avm.api.defkernel)


;;
;; DEFKERNEL

(defun compute-ranges (n size)
  (let ((size1 (ceiling (/ size n))))
    (let (ranges)
      (do ((begin 0 (+ begin size1))
           (end size1 (min (+ end size1) size)))
          ((>= begin size) (nreverse ranges))
        (push (list begin end) ranges)))))

(defun compute-dimension (args)
  (let ((arrays (remove-if-not #'array-p args)))
    (or (and arrays
             (array-size (car arrays)))
      (error "SIZE required if no array arguments."))))

(defun array-lisp-bindings (args1 args)
  (loop for arg1 in args1
        for arg in args
     collect `(,arg1 (if (array-p ,arg)
                         (avm.api.array::array-tuple-array ,arg)
                         ,arg))))

(defun array-cuda-bindings (args1 args)
  (loop for arg1 in args1
        for arg in args
     collect `(,arg1 (if (array-p ,arg)
                         (avm.api.array::array-device-ptr ,arg)
                         ,arg))))

(defvar *number-of-threads* 1)

(defun defun-entry-function-form (name lisp-name cuda-name args)
  (let ((args1 (map-into (make-list (length args)) #'gensym))
        (arg (gensym)))
    `(defun ,name (,@args &key size)
       (let ((n (or size (compute-dimension (list ,@args)))))
         (declare (type fixnum n))
         (cond
           ((cuda-state-used-p)
            ;; Synchronize arrays appearing in arguments.
            (loop for ,arg in (list ,@args)
               when (array-p ,arg)
               do (array-ensure-cuda-up-to-date ,arg)
                  (array-set-cuda-dirty ,arg))
            ;; Launch kernel.
            (let ,(array-cuda-bindings args1 args)
              (let ((grid-dim (list (ceiling n 64) 1 1))
                    (block-dim '(64 1 1)))
                (,cuda-name n ,@args1 :grid-dim grid-dim
                                      :block-dim block-dim))))
           ((< 1 *number-of-threads*)
            ;; Synchronize arrays appearing in arguments.
            (when (cuda-available-p)
              (loop for ,arg in (list ,@args)
                 when (array-p ,arg)
                 do (array-ensure-lisp-up-to-date ,arg)
                    (array-set-lisp-dirty ,arg)))
            ;; Launch kernel.
            (let (,@(array-lisp-bindings args1 args)
                  (ranges (compute-ranges *number-of-threads* n)))
              (let (threads)
                (dolist (range ranges)
                  (destructuring-bind (begin end) range
                    (push (bt:make-thread
                           #'(lambda ()
                               (loop for i from begin below end
                                  do (,lisp-name i n ,@args1))))
                          threads)))
                (loop for thread in threads
                   do (bt:join-thread thread)))))
           (t
            ;; Synchronize arrays appearing in arguments.
            (when (cuda-available-p)
              (loop for ,arg in (list ,@args)
                 when (array-p ,arg)
                 do (array-ensure-lisp-up-to-date ,arg)
                    (array-set-lisp-dirty ,arg)))
            ;; Launch kernel.
            (let ,(array-lisp-bindings args1 args)
              (dotimes (i n)
                (declare (type fixnum i))
                (,lisp-name i n ,@args1)))))))))

(defun defkernel-form (manager name args body)
  (multiple-value-bind (lisp-name lisp-form cuda-name cuda-form
                        include-vector-type-p)
      (kernel-manager-define-function manager name args body)
    (let ((entry-function-form
           (defun-entry-function-form name lisp-name cuda-name args)))
      `(progn
         ;; Define Lisp kernel.
         ,lisp-form
         ;; Define CUDA kernel.
         ,cuda-form
         ;; Define entry function.
         ,@(when (not include-vector-type-p)
             (list entry-function-form))))))

(defvar *compile-on-runtime* nil)

(defmacro defkernel (name args body)
  (if (not *compile-on-runtime*)
      (defkernel-form *kernel-manager* name args body)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (eval
          (defkernel-form *kernel-manager* ',name ',args ',body)))))
