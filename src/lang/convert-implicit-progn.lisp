#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm.lang.convert-implicit-progn
  (:use :cl
        :avm.lang.syntax)
  (:import-from :alexandria
                :with-gensyms)
  (:export :convert-implicit-progn))
(in-package :avm.lang.convert-implicit-progn)


(defun convert-implicit-progn (forms)
  (destructuring-bind (form1 . forms1) forms
    (if forms1
        (with-gensyms (var)
          `(let ((,var ,(convert-form form1)))
             ,(convert-forms forms1)))
        (convert-form form1))))

(defun convert-form (form)
  (cond
    ((literal-p form) form)
    ((reference-p form) form)
    ((accessor-p form) (convert-accessor form))
    ((the-p form) (convert-the form))
    ((if-p form) (convert-if form))
    ((let-p form) (convert-let form))
    ((flet-p form) (convert-flet form))
    ((labels-p form) (convert-labels form))
    ((set-p form) (convert-set form))
    ((apply-p form) (convert-apply form))
    (t (error "The value ~S is an invalid form." form))))

(defun convert-let (form)
  (let ((bindings (let-bindings form))
        (body (let-body-progn form)))
    (let ((bindings1 (loop for (var value) in bindings
                           for value1 = (convert-form value)
                        collect `(,var ,value1)))
          (body1 (convert-implicit-progn body)))
      `(let ,bindings1 ,body1))))

(defun convert-flet (form)
  (let ((bindings (flet-bindings form))
        (body (flet-body form)))
    (%convert-flet 'flet bindings body)))

(defun %convert-flet (op bindings body)
  (let ((bindings1 (loop for (name args body) in bindings
                         for body1 = (convert-forms body)
                      collect `(,name ,args ,body1)))
        (body1 (convert-forms body)))
    `(,op ,bindings1 ,body1)))
