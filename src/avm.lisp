#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm
  (:use :cl)
  (:export ;; Types
           :bool
           :int :int2 :int3 :int4
           :float :float2 :float3 :float4
           :double :double2 :double3 :double4
           :int-array :int2-array :int3-array :int4-array
           :float-array :float2-array :float3-array :float4-array
           :double-array :double2-array :double3-array :double4-array
           ;; Vector type accessors
           :int2-x :int2-y
           :int3-x :int3-y :int3-z
           :int4-x :int4-y :int4-z :int4-w
           :float2-x :float2-y
           :float3-x :float3-y :float3-z
           :float4-x :float4-y :float4-z :float4-w
           :double2-x :double2-y
           :double3-x :double3-y :double3-z
           :double4-x :double4-y :double4-z :double4-w           
           ;; Vector algebra
           :*. :.* :/. :dot
           ;; Mathematical functions
           :rsqrt
           ;; Built-in variables
           :i :n
           ;; Special variables
           :*kernel-manager*
           ;; DEFKERNEL
           :defkernel
           :*number-of-threads*
           :*compile-on-runtime*
           ;; CUDA
           :with-cuda
           :*use-cuda-p*
           :synchronize
           ;; Arrays
           :avm-array
           :array-p
           :alloc-array
           :free-array
           :with-array
           :with-arrays
           :array-aref
           :array-size
           ))
(in-package :avm)

;; blah blah blah.
