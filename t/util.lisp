#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm-test.util
  (:use :cl)
  (:export :replace-gensym
           :_))
(in-package :avm-test.util)


(defun replace-gensym (form)
  (if (atom form)
      (if (and (symbolp form)
               (null (symbol-package form)))
          '_
          form)
      (mapcar #'replace-gensym form)))
