# AVM - Arrayed Vector Math library

Efficient and expressive arrayed vector math library with multi-threading and CUDA support in Common Lisp.

```common-lisp
(defkernel mandelbrot (xs)
  (labels ((aux (x y a b m)
             (if (< m 100)
                 (let ((x1 (- (* x x) (* y y) a))
                       (y1 (- (* 2.0 x y) b)))
                   (if (> (+ (* x1 x1) (* y1 y1)) 4.0)
                       m
                       (aux x1 y1 a b (+ m 1))))
                 0)))
    (let ((a (/ (coerce (- (mod i 2048) 512)) 1024.0))
          (b (/ (coerce (- (/ i 2048) 1024)) 1024.0)))
      (set (aref xs i) (aux 0.0 0.0 a b 1)))))

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
      (mandelbrot xs)
      (draw-mandelbrot #P"./mandelbrot.pgm" xs))))
```

## Usage

To be described.

## Installation

To be described.

## API

### [Macro] defkernel

    DEFKERNEL

To be described.

### [Macro] defkernel-macro

    DEFKERNEL-MACRO
    
To be described.

### [Macro] defkernel-global

    DEFKERNEL-GROBAL

To be described.

### [Macro] defkernel-constant

    DEFKERNEL-CONSTANT

To be described.

### [Macro] defkernel-symbol-macro

    DEFKERNEL-SYMBOL-MACRO

To be described.

### [Macro] with-array, with-arrays

    WITH-ARRAY
    WITH-ARRAYS

To be described.

### [Function] alloc-array

    ALLOC-ARRAY

To be described.

### [Function] free-array

    FREE-ARRAY

To be described.

### [Function] array-size

    ARRAY-SIZE

To be described.

### [Accessor] array-aref

    ARRAY-AREF
    SETF ARRAY-AREF

To be described.

### [Macro] with-cuda

    WITH-CUDA (dev-id) &body body

To be described.

### [Function] synchronize

    SYNCHRONIZE

Explicitly synchronizes CUDA context with `cl-cuda:synchronize-context`. This function is useful in case that you want to get timing data of CUDA kernel launching with `time` macro because CUDA kernel functions are executed asynchronously so it pass through `time` form in a moment without it. It does nothing if CUDA is not used.

**Example:**

    (with-cuda (0)
      (time
        (progn
          (do-something)
          (synchronize))))

### [Special Variable] \*use-thread-p\*

To be described.

## Kernel Description Language

### [Type] int, int2, int3, int4

To be described.

### [Type] float, float2, float3, float4

To be described.

### [Type] double, double2, double3, double4

To be described.

### [Syntax] the

    THE type value

To be described.

### [Syntax] if

    IF test-form then-form else-form

To be described.

### [Syntax] let

    LET

To be described.

### [Syntax] flet

    FLET

To be described.

### [Syntax] labels

    LABELS

To be described.

### [Syntax] set

    SET place value

To be described.

### [Built-in Variable] i

To be described.

### [Built-in Variable] n

To be described.

### [Built-in Function] +, -, *, /, mod

To be described.

### [Built-in Function] \*., .\*, /.

To be described.

### [Built-in Function] <, >

To be described.

### [Built-in Macro] ...

To be described.

## Author

* Masayuki Takagi (kamonama@gmail.com)

## Copyright

Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)

## License

Licensed under the MIT License.
