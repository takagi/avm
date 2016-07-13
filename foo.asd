#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo-asd
  (:use :cl :asdf))
(in-package :foo-asd)

(defsystem foo
  :version "0.1"
  :author "Masayuki Takagi"
  :license "MIT"
  :depends-on (:cl-tuples
               :cl-pattern
               :cl-unification
               :cl-cuda
               :bordeaux-threads)
  :components ((:module "src"
                :serial t
                :components
                ((:file "foo")
                 (:module "lang"
                  :serial t
                  :components
                  ((:file "symbol")
                   (:file "data")
                   (:file "type")
                   (:file "syntax")
                   (:file "built-in")
                   (:file "binarize")
                   (:file "convert-functions")
                   (:file "free-variable")
                   (:file "unienv")
                   (:file "typenv")
                   (:file "appenv")
                   (:file "funenv")
                   (:file "infer")
                   (:file "kernel")
                   (:file "lang")
                   (:module "compiler"
                    :serial t
                    :components
                    ((:module "cl"
                      :serial t
                      :components
                      ((:file "built-in")
                       (:file "compile-type")
                       (:file "varenv")
                       (:file "compile-form")
                       (:file "lang")))))))
                 (:module "api"
                  :serial t
                  :components
                  ((:file "cuda")
                   (:file "array")
                   (:file "kernel-manager")
                   (:file "defkernel"))))))
  :description "Efficient and expressive arrayed vector math library."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op foo-test))))
