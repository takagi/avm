#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm-test
  (:use :cl
        :avm
        :prove))
(in-package :avm-test)

;; NOTE: To run this test file, execute `(asdf:test-system :avm)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)
