#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.samples.mandelbrot
  (:use :cl
        :avm)
  (:export :main))
(in-package :avm.samples.mandelbrot)


;(defkernel-symbol-macro nmax 100)

(defkernel mandelbrot (xs)
  (labels ((aux (x y a b m)
             (if (< m 100)
                 (let ((x1 (- (* x x) (* y y) a))
                       (y1 (- (* 2.0 x y) b)))
                   (if (> (+ (* x1 x1) (* y1 y1)) 4.0)
                       m
                       (aux x1 y1 a b (+ m 1))))
                 0)))
    (let ((a (/ (float (- (mod i 2048) 512)) 1024.0))
          (b (/ (float (- (/ i 2048) 1024)) 1024.0)))
      (setf (aref xs i) (aux 0.0 0.0 a b 1)))))

(defun draw-mandelbrot (pathname xs)
  (with-open-file (out pathname :direction :output
                                :if-does-not-exist :create
                                :if-exists :supersede)
    (write-line "P2" out)
    (write-line "2048 2048" out)
    (write-line "255" out)
    (dotimes (i (* 2048 2048))
      (princ (min 255 (* 8 (array-aref xs i))) out)
      (terpri out))))

(defun main (&optional dev-id)
  (with-cuda (dev-id)
    (with-arrays ((xs int (* 2048 2048)))
      (time (progn
       (mandelbrot xs)
       (synchronize)))
      (draw-mandelbrot #P"mandelbrot.pgm" xs))))
