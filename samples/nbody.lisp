#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.samples.nbody
  (:use :cl
        :avm)
  (:export :main))
(in-package :avm.samples.nbody)


(defkernel acceleration1 (xi xj)
  (let* ((r (float4 (- (float4-x xj) (float4-x xi))
                    (- (float4-y xj) (float4-y xi))
                    (- (float4-z xj) (float4-z xi))
                    0.0))
         (dist-sqr (+ (dot r r)
                      (* 0.1 0.1)))     ; add softening factor
         (inv-dist (rsqrt dist-sqr))
         (inv-dist-cube (* inv-dist inv-dist inv-dist))
         (scale (* (float4-w xj) inv-dist-cube)))
    (*. r scale)))

(defkernel acceleration (xi xs0)
  ;; TODO: ITER macro
  ;; TODO: Accept free variables
  (labels ((aux (xi xs0 j a)
             (if (< j n)
                 (let ((xj (aref xs0 j)))
                   (aux xi xs0 (+ j 1) (+ a (acceleration1 xi xj))))
                 a)))
    (aux xi xs0 0 (float4 0.0 0.0 0.0 0.0))))

(defkernel integrate-bodies (xs xs0 vs dt)
  ;; Compute acceleration.
  (let* ((x (aref xs0 i))
         (a (acceleration x xs0)))
    ;; Update velocity.
    (setf (aref vs i) (+ (aref vs i) (*. a dt)))
    ;; Update position.
    (setf (aref xs i) (+ x (*. (aref vs i) dt)))))

(defun initialize (xs vs)
  (dotimes (i (array-size xs))
    (let ((x (- (random 2.0) 1.0))
          (y (- (random 2.0) 1.0))
          (z (- (random 2.0) 1.0)))
      (setf (array-aref xs i) (values x y z 1.0)))
    (setf (array-aref vs i) (values 0.0 0.0 0.0 0.0))))

(defun plot (img x y)
  ;; TODO: Use gaussian.
  (loop for i from (- x 5) below (+ x 5) do
     (loop for j from (- y 5) below (+ y 5)
        when (and (<= 0 i 1023)
                  (<= 0 j 1023))
          do (setf (aref img i j) 255))))

(defun output-bodies (pathname xs)
  (let ((img (make-array '(1024 1024) :element-type 'fixnum
                                      :initial-element 0)))
    ;; Make image map from positions.
    (dotimes (i (array-size xs))
      (multiple-value-bind (x y z w) (array-aref xs i)
        (declare (ignore z w))
        (let ((x1 (truncate (* (+ x 1.0) 512.0)))
              (y1 (truncate (* (+ y 1.0) 512.0))))
          (plot img x1 y1))))
    ;; Output image map.
    (with-open-file (out pathname :direction :output
                                  :if-does-not-exist :create
                                  :if-exists :supersede)
      (write-line "P2" out)
      (write-line "1024 1024" out)
      (write-line "255" out)
      (dotimes (i 1024)
        (dotimes (j 1024)
          (princ (aref img i j) out)
          (princ " " out))
        (terpri out)))))

(defun main (&optional dev-id)
  (let ((n 32768)
        (dt 0.001))
    (with-cuda (dev-id)
      (with-arrays ((xs float4 n)
                    (xs0 float4 n)
                    (vs float4 n))
        ;; Initialize poistion and velocity.
        (initialize xs vs)
        ;; Simulate bodies.
        (time (progn
         (loop repeat 10
            do (rotatef xs xs0)
               (integrate-bodies xs xs0 vs dt))
         (synchronize)))
        ;; Output bodies.
        (output-bodies "nbody.pgm" xs)))))
