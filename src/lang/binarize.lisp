#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.binarize
  (:use :cl
        :avm
        :avm.lang.built-in)
  (:export :binarize))
(in-package :avm.lang.binarize)


;;
;; Binarize

(defun binarize (form)
  (if (atom form)
      form
      (if (and (nthcdr 3 form)
               (built-in-arithmetic-p (car form)))
          (if (built-in-arithmetic-left-assoc-p (car form))
              (destructuring-bind (op a1 a2 . rest) form
                (binarize `(,op (,op ,(binarize a1) ,(binarize a2)) ,@rest)))
              (destructuring-bind (op . args) form
                (destructuring-bind (a2 a1 . rest) (reverse args)
                  (binarize `(,op ,@(reverse rest)
                                  (,op ,(binarize a1) ,(binarize a2)))))))
          (mapcar #'binarize form))))
