#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm-samples-asd
  (:use :cl :asdf))
(in-package :avm-samples-asd)

(defsystem avm-samples
  :author "Masayuki Takagi"
  :license "MIT"
  :depends-on (:avm)
  :components ((:module "samples"
                :components
                ((:file "mandelbrot"))))
  :description "Sample codes for avm"

  :perform (load-op :after (op c) (asdf:clear-system c)))
