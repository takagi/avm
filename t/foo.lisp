#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo-test
  (:use :cl
        :foo
        :prove))
(in-package :foo-test)

;; NOTE: To run this test file, execute `(asdf:test-system :foo)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)
