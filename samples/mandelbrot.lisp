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

(defkernel mandelbrot (xs x y m)
  (let ((a (/ (coerce (- (mod i 2048) 512)) 1024.0d0))
        (b (/ (coerce (- (/ i 2048) 1024)) 1024.0d0)))
    (if (< m 100)
        (let ((x1 (- (* x x) (* y y) a))
              (y1 (- (* 2.0d0 x y) b)))
          (if (> (+ (* x1 x1) (* y1 y1)) 4.0d0)
              (set (aref xs i) m)
              (mandelbrot xs x1 y1 (+ m 1))))
        (set (aref xs i) 0))))

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
      (time
       (mandelbrot xs 0.0d0 0.0d0 1))
      (draw-mandelbrot #P"~/Desktop/mandelbrot.pgm" xs))))

#+nil
(eval-when (:compile-toplevel :load-toplevel)
  (require :sb-sprof))
#+nil
(defun profile (&optional dev-id)
  (sb-sprof:with-profiling (:max-samples 1000
                            :report      :graph
                            :loop        nil)
    (main dev-id)))
