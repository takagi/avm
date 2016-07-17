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

    DEFKERNEL name args body

Defines a kernel function. A defined kernel function is callable as if it is an ordinal Common Lisp function except that it may accept `:size` keyword parameter that specifies array size to be processed. If it is not given, the size of the first array in arguments is used. The `:size` keyword parameter is for the case that you want to pass arrays with various sizes and use the size of array that does not appear first in arguments. You may use built-in variables `i` and `n` in kernel functions, which contain the index and the size of array the kernel function processes respectively.

**Example:**

```common-lisp
(defkernel fill-one (xs a)
  (set (aref xs i) 1))

(with-array (xs int 1000)
  (fill-one xs))
```

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

    ARRAY-SIZE array => size

Returns the size of given array.

**Example:**

```common-lisp
(with-array (xs int 100)
  (array-size xs))
=> 100
```

### [Accessor] array-aref

    ARRAY-AREF
    SETF ARRAY-AREF

To be described.

### [Macro] with-cuda

    WITH-CUDA (dev-id) &body body

To be described.

### [Function] synchronize

    SYNCHRONIZE

Explicitly synchronizes CUDA context with `cl-cuda:synchronize-context`. This function is useful in case that you want to get timing data of CUDA kernel launching with `time` macro because CUDA kernel functions are executed asynchronously so it passes through `time` form in a moment without it. It does nothing if CUDA is not available.

**Example:**

```common-lisp
(with-cuda (0)
  (time
    (progn
      (do-something)
      (synchronize))))
```

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

    THE type value => result

To be described.

### [Syntax] if

    IF test-form then-form else-form => result

`if` allows the evaluation of a form to be dependent on a single `test-form`. First `test-form` is evaluated. If the result is `true`, then `then-form` is selected; otherwise `else-form` is selected. Whichever form is selected is then evaluated and its result value returns.

**Example:**

```common-lisp
(let ((a 1))
  (if (= a 1)
      42
      0))
=> 42
```

### [Syntax] let

    LET ({(var value)}*) body => result

`let` declares new variable bindings and set corresponding `value`s to them and evaluates `body` form that uses these bindings and returns its result. `let` performs the bindings in parallel. For sequentially, use `let*` kernel macro instead.

**Example:**

```common-lisp
(let ((x 42))
  x)
=> 42
```

### [Syntax] flet

    FLET ({(name args local-form)}*) body => result

`flet` defines locally named functions and evaluates its `body` with these definition bindings. Any number of such local functions can be defined. The scope of the name binding encompasses only the body. You may use built-in variables `i` and `n` in local functions as well as global kernel functions.

**Example:**

```common-lisp
(flet ((aux (x) (+ x 1)))
   (aux 10))
=> 11
```

### [Syntax] labels

    LABELS ({(name args local-form)}*) body => result

`labels` is equivalent to `flet` except that the scope of the defined function names for `labels` encompasses the function definition themselves as well as the body.

**Examples:**

```common-lisp
(labels ((aux (x y)
           (if (= x 0)
               y
               (aux (- x 1) (+ y 1)))))
  (aux 10 0))
=> 10
```

### [Syntax] set

    SET place value => result

`set` changes the value of `place` to be `value` and returns `value` as its result. Accessor forms are acceptable as `place`s.

**Example:**

```
(set (aref xs i) (+ (aref xs i) (int2 1 1)))
=> (int2 1 1)
```

### [Built-in Function] int2, int3, int4

To be described.

### [Accessor] int2-x, int2-y, int3-x, int3-y, int3-z, int4-x, int4-y, int4-z, int4-w

To be described.

### [Built-in Function] float2, float3, float4

To be described.

### [Accessor] float2-x, float2-y, float3-x, float3-y, float3-z, float4-x, float4-y, float4-z, float4-w

To be described.

### [Built-in Function] double2, double3, double4

To be described.

### [Accessor] double2-x, double2-y, double3-x, double3-y, double3-z, double4-x, double4-y, double4-z, double4-w

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

## Concepts

### CUDA state

AVM has the following three states relevant to CUDA:

- Not Available
- Available
- Used

And here shows CUDA state's transfer diagram:

```
WITH-CUDA (nil)               WITH-CUDA (N)
    +-----+      +--------------------------------------+
    |     |      |                                      |
    |     |      |                    *use-cuda-p* nil  |
    |     |      |                     <-------------   v
    +-> Not Available        Available                Used
                                       ------------->
                                    *use-cuda-p* not nil
```

The initial state is "Not Available". When CUDA state is "Not Available", AVM does not use CUDA. When AVM has this state is that actually CUDA is not available on your machine, out of `with-cuda` context or in `with-cuda` context with its `dev-id` parameter `nil`.

When CUDA state is "Available", AVM is ready to use CUDA with initializing it and creating CUDA context as well as allocating device memory on `alloc-array`ing, though kernel functions are actually not executed on CUDA in this state. When AVM has this state is that CUDA is available on your machine within `with-cuda` context with its `dev-id` parameter an integer that indicates which GPU you use and `*use-cuda-p*` special variable is set to `nil`.

When CUDA state is "Used", AVM is ready to use CUDA as well as when CUDA state is "Available" and kernel functions are actually executed on CUDA. When AVM has this state is same as when CUDA state is "Available" except that `*use-cuda-p*` special variable is set to not `nil`, which is the default value of that in `with-cuda` context.

## Author

* Masayuki Takagi (kamonama@gmail.com)

## Copyright

Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)

## License

Licensed under the MIT License.
