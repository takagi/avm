#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo-test-asd
  (:use :cl :asdf))
(in-package :foo-test-asd)

(defsystem foo-test
  :author "Masayuki Takagi"
  :license "MIT"
  :depends-on (:foo
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "foo"))))
  :description "Test system for foo"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
            (funcall (intern #.(string :run-test-system) :prove-asdf) c)
            (asdf:clear-system c)))
