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

##Benchmark

AVM's kernel functions run **almost as fast as equivalent C/C++ codes** with SBCL Common Lisp compiler. And we can easily make them run in parallel with just specifying the number of threads we use. Here shows a benchmark of computing 2048x2048 Mandelbrot set and 32768 bodies N-body simulation.

![Benchmark](https://docs.google.com/spreadsheets/d/1_-_ucZTxqWXt1lqLOBoNQnMF5Us6ft3UMH9GT2wxWTM/pubchart?oid=414899701&format=image)

Additionally, AVM provides Nvidia CUDA support so we can enormously accelerate computing kernel functions with GPUs.

![Benchmark with CUDA](https://docs.google.com/spreadsheets/d/1_-_ucZTxqWXt1lqLOBoNQnMF5Us6ft3UMH9GT2wxWTM/pubchart?oid=1016247054&format=image)

These benchmarks are measured on the following environment:

- gcc 4.8.4
- SBCL 1.3.3
- Intel Celeron CPU G3900 @ 2.80GHz
- Nvidia GeForce GTX 750 Ti
- Linux ubuntu 4.2.0-41-generic x86_64

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

### [Special Variable] \*compile-on-runtime\*

Delays AVM kernel function compilation to runtime if not `nil`. Otherwise, AVM kernel functions are compiled at compile time. Delaying compilation to runtime is useful, at least on SBCL, for debuging AVM kernel definitions because it makes CL debugger show backtrace when they have some errors.

**Example:**

```common-lisp
(setf *compile-on-runtime* t)
(defkernel some-error ()
  (+ 1 1.0))                            ; AVM compile error showing backtrace
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

### [Function] alloc-array

    ALLOC-ARRAY type size => array

Allocates an AVM array with specified `type` and `size`. `type` is a AVM type that means the array's base type and `size` is a positive integer that indicates the size of the array. For now, AVM supports one-dimensional array only. If CUDA is not available, memory area for CUDA is not allocated. `alloc-array`ed array should be freed with `free-array`. For convenience, `with-array` macro and `with-arrays` macro are provided.

**Example:**

```common-lisp
(with-cuda (0)
  (let ((xs (alloc-array 'int 100)))
    (unwind-protect
        (do-something-with xs)
      (free-array xs))))
```

### [Function] free-array

    FREE-ARRAY array => No value

Frees the given `array`. Does nothing if `array` is already freed.

**Example:**

```common-lisp
(with-cuda (0)
  (let ((xs (alloc-array 'int 100)))
    (unwind-protect
        (do-something-with xs)
      (free-array xs))))
```

### [Macro] with-array, with-arrays

    WITH-ARRAY
    WITH-ARRAYS

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

### [Special Variable] \*use-cuda-p\*

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

### [Special Variable] \*number-of-threads\*

To be described.

## Kernel Description Language

### [Type] int, int2, int3, int4

Integer type `int` and its derived vector types.

### [Type] float, float2, float3, float4

Single precision floating point type `float` and its derived vector types.

### [Type] double, double2, double3, double4

Double precision floating point type `double` and its derived vector types.

### [Syntax] the

    THE type form => result

`the` specifies the value returned by `form` is of the type specified by `type`.

**Example:**

```common-lisp
(flet ((identity (x)
         (the int x)))
  (identity 42))
=> 42
```

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

```common-lisp
(set (aref xs i) (+ (aref xs i) (int2 1 1)))
=> (int2 1 1)
```

### [Built-in Function] int2, int3, int4

    INT2 form form => result
    INT3 form form form => result
    INT4 form form form form => result

`int2`, `int3` and `int4` provide constructors for `int` derived vector types. Each parameter form should have type of `int`.

**Example:**

```common-lisp
(int2 0 0) => (int2 0 0)
(int3 0 0 0) => (int3 0 0 0)
(int4 0 0 0 0) => (int4 0 0 0 0)
```

### [Built-in Function] float2, float3, float4

    FLOAT2 form form => result
    FLOAT3 form form form => result
    FLOAT4 form form form form => result

`float2`, `float3` and `flaot4` provide constructors for `float` derived vector types. Each parameter form should have type of `float`.

**Example:**

```common-lisp
(float2 0.0 0.0) => (float2 0.0 0.0)
(float3 0.0 0.0 0.0) => (float3 0.0 0.0 0.0)
(float4 0.0 0.0 0.0 0.0) => (float4 0.0 0.0 0.0 0.0)
```

### [Built-in Function] double2, double3, double4

    DOUBLE2 form form => result
    DOUBLE3 form form form => result
    DOUBLE4 form form form form => result

`double2`, `double3` and `double4` provide constructors for `double` derived vector types. Each parameter form should have type of `double`.

**Example:**

```common-lisp
(double2 0.0d0 0.0d0) => (double2 0.0d0 0.0d0)
(double3 0.0d0 0.0d0 0.0d0) => (double3 0.0d0 0.0d0 0.0d0)
(double4 0.0d0 0.0d0 0.0d0 0.0d0) => (double4 0.0d0 0.0d0 0.0d0 0.0d0)
```

### [Accessor] int2-x, int2-y, int3-x, int3-y, int3-z, int4-x, int4-y, int4-z, int4-w

    INT2-X form => result
    INT2-Y form => result
    INT3-X form => result
    INT3-Y form => result
    INT3-Z form => result
    INT4-X form => result
    INT4-Y form => result
    INT4-Z form => result
    INT4-W form => result

Accesses each component of `int` derived vector types. The type of `form` should be of each accessor's corresponding vector type. The type of result is `int`. You can read its value as well as destructively set it with SET form.

**Example:**

```common-lisp
(int2-x (int2 1 2)) => 1
(let ((x (int2 1 2)))
  (set (int2-x x) 3)
  x)
=> (int2 3 2)
```

### [Accessor] float2-x, float2-y, float3-x, float3-y, float3-z, float4-x, float4-y, float4-z, float4-w

    FLOAT2-X form => result
    FLOAT2-Y form => result
    FLOAT3-X form => result
    FLOAT3-Y form => result
    FLOAT3-Z form => result
    FLOAT4-X form => result
    FLOAT4-Y form => result
    FLOAT4-Z form => result
    FLOAT4-W form => result

Accesses each component of `float` derived vector types. The type of `form` should be of each accessor's corresponding vector type. The type of result is `float`. You can read its value as well as destructively set it with SET form.

**Example:**

```common-lisp
(float2-x (float2 1.0 2.0)) => 1.0
(let ((x (float2 1.0 2.0)))
  (set (float2-x x) 3.0)
  x)
=> (float2 3.0 2.0)
```

### [Accessor] double2-x, double2-y, double3-x, double3-y, double3-z, double4-x, double4-y, double4-z, double4-w

    DOUBLE2-X form => result
    DOUBLE2-Y form => result
    DOUBLE3-X form => result
    DOUBLE3-Y form => result
    DOUBLE3-Z form => result
    DOUBLE4-X form => result
    DOUBLE4-Y form => result
    DOUBLE4-Z form => result
    DOUBLE4-W form => result

Accesses each component of `double` derived vector types. The type of `form` should be of each accessor's corresponding vector type. The type of result is `double`. You can read its value as well as destructively set it with SET form.

**Example:**

```common-lisp
(double2-x (double2 1.0d0 2.0d0)) => 1.0d0
(let ((x (double2 1.0d0 2.0d0)))
  (set (double2-x x) 3.0d0)
  x)
=> (double2 3.0d0 2.0d0)
```

### [Accessor] aref

    AREF array index => result

Accesses the `array` element specified by the `index`. The type of `array` is an array type with base type of `int`, `float`, `double` and their derived vector types. The type of `index` is `int`, and the type of `result` is the base type. You can read its value as well as destructively set it with SET form.

**Example:**

```common-lisp
(aref xs 0) => 1
(set (aref xs 0) 1) => 1
(aref ys 0) => (int2 1 1)
(set (aref ys 0) (int2 2 2)) => 2
```

### [Built-in Variable] i, n

To be described.

### [Built-in Function] +, -, *, /, mod

To be described.

### [Built-in Function] \*., .\*, /., dot

To be described.

### [Built-in Function] <, >

To be described.

### [Built-in Function] rsqrt

    RSQRT x => result

These built-in functions provide mathematical functions.

### [Built-in Macro] ...

To be described.

## Concepts

### Array

The array in AVM is an abstract data structure that consists of a memory area for computation on Lisp and another for computation on Nvidia CUDA.

- Memory area for computation on Lisp
  - Tuple array
- Memory area for computation on CUDA
  - Host memory
  - Device memory

Eacn AVM thread of kernel functions reads from and writes to arrays. Arrays are passed to kernel fucntions on launching them.

`alloc-array` allocates an array and it should be freed with `free-array`. For convenience, `with-array` macro and `with-arrays` macro are provided. `array-aref` accessor is used to read and write a value to an array. A value of arrays whose base type is a vector type is accessed via values.

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

### Array states

AVM's arrays have the following state variables:

- CUDA availability
- CUDA availability on allocation
- Freed or not

CUDA availability is if CUDA is available or not when array operations are called. CUDA availability on allocation is if CUDA was available or not when arrays are `alloc-array`ed. Free or not is if arrays are already `free-array`ed or not.

How array operations work is dependent of these state variables. For detail, see each array operation's API description.

### Multi-threding state

To be described.

## Author

* Masayuki Takagi (kamonama@gmail.com)

## Copyright

Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)

## License

Licensed under the MIT License.
