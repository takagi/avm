#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm-test-asd
  (:use :cl :asdf))
(in-package :avm-test-asd)

(defsystem avm-test
  :author "Masayuki Takagi"
  :license "MIT"
  :depends-on (:avm
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "avm")
                 (:file "util")
                 (:module "lang"
                  :serial t
                  :components
                  ((:test-file "expand-macro")
                   (:test-file "convert-implicit-progn")
                   (:test-file "convert-functions")
                   (:test-file "free-variable")
                   (:module "compiler"
                    :serial t
                    :components
                    ((:module "lisp"
                      :serial t
                      :components
                      ((:test-file "compile")))
                     (:module "cuda"
                      :serial t
                      :components
                      ((:test-file "k-normal")
                       (:test-file "compile")))
                     ))))
                 (:module "api"
                  :serial t
                  :components
                  ((:test-file "cuda")
                   (:test-file "array"))))))
  :description "Test system for avm"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
            (funcall (intern #.(string :run-test-system) :prove-asdf) c)
            (asdf:clear-system c)))
