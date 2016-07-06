#|
  This file is a part of foo project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage foo.lang.funenv
  (:use :cl)
  (:export :empty-funenv))
(in-package :foo.lang.funenv)


;;
;; Function environment

(defun empty-funenv ()
  nil)
