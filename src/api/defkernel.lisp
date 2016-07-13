#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo.api.defkernel
  (:use :cl
        :foo
        :foo.api.array
        :foo.api.kernel-manager)
  (:export :defkernel
           :*use-thread-p*))
(in-package :foo.api.defkernel)


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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun array-lisp-bindings (args1 args)
    (loop for arg1 in args1
       for arg in args
       collect `(,arg1 (if (array-p ,arg)
                           (array-lisp ,arg)
                           ,arg)))))

(defvar *use-thread-p* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun defkernel-entry-form (name cl-name args)
    (let ((args1 (map-into (make-list (length args)) #'gensym))
          (arg (gensym)))
      `(defun ,name (,@args &key size)
         (let ((n (or size
                      (compute-dimension (list ,@args)))))
           (declare (type fixnum n))
           (cond
             (*use-cuda-p* nil)
             (*use-thread-p*
              ;; Synchronize arrays appearing in arguments.
              (loop for ,arg in (list ,@args)
                 when (array-p ,arg)
                 do (array-ensure-lisp-up-to-date ,arg)
                    (set-array-dirty ,arg :lisp))
              ;; Launch kernel.
              (let (,@(array-lisp-bindings args1 args)
                    (ranges (compute-ranges 2 n)))
                (let (threads)
                  (dolist (range ranges)
                    (destructuring-bind (begin end) range
                      (push (bt:make-thread
                             #'(lambda ()
                                 (loop for i from begin below end
                                    do (,cl-name i n ,@args1))))
                            threads)))
                  (loop for thread in threads
                     do (bt:join-thread thread)))))
             (t
              ;; Synchronize arrays appearing in arguments.
              (loop for ,arg in (list ,@args)
                 when (array-p ,arg)
                 do (array-ensure-lisp-up-to-date ,arg)
                    (set-array-dirty ,arg :lisp))
              ;; Launch kernel.
              (let ,(array-lisp-bindings args1 args)
                (dotimes (i n)
                  (declare (type fixnum i))
                  (,cl-name i n ,@args1))))))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun defkernel-form (manager name args body)
    (multiple-value-bind (cl-name include-vector-type-p cl-form)
        (kernel-manager-define-function manager name args body)
      `(progn
         ;; Define CL kernel.
         ,cl-form
         ;; Define CUDA kernel.
         nil
         ;; Define entry point.
         ,(when (not include-vector-type-p)
            (defkernel-entry-form name cl-name args))))))

(defmacro defkernel (name args body)
  (defkernel-form *kernel-manager* name args body))
