#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo.samples.mandelbrot
  (:use :cl)
  (:export :main))
(in-package :foo.samples.mandelbrot)


(defkernel-symbol-macro nmax 100)

(defkernel mandelbrot (xs x y n)
  (if (< n nmax)
      (let ((x1 (- (* x x) (* y y) i))
            (y1 (- (* 2.0d0 x y) j)))
        (if (> (+ (* x1 x1) (* y1 y1)) 4.0d0)
            (set (aref xs i j) n)
            (mandelbrot xs x y (+ n 1))))
      (set (aref i j) 0)))

(defun draw-mandelbrot (xs)
  (declare (ignore xs))
  (error "Not implemented."))

(defun main ()
  (with-cuda (dev-id)
    (with-arrays ((xs int (32 32)))
      (mandelbrot xs 0.0d0 0.0d0 1)
      (draw-mandelbrot xs))))
