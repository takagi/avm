#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm-test.api.macro
  (:use :cl
        :prove
        :avm
        :avm.api.macro))
(in-package :avm-test.api.macro)


(plan nil)


(defun replace-gensym (form)
  (if (atom form)
      (if (and (symbolp form)
               (null (symbol-package form)))
          '_
          form)
      (mapcar #'replace-gensym form)))


;;
;; PROGN

(subtest "progn"

    (is (replace-gensym
         (expand-macro '(progn 1 2 3)))
        '(let ((_ 1))
           (progn 2 3))
        "Base case - multiple forms.")

    (is (expand-macro '(progn 1))
        1
        "Base case - only a form.")

    (is-error (expand-macro '(progn))
              simple-error
              "Invalid PROGN form."))


;;
;; LET*

(subtest "let*"

  (is (expand-macro '(let* ((x 1)
                            (y (+ x 1)))
                       (+ x y)))
      '(let ((x 1))
         (let ((y (+ x 1)))
           (progn (+ x y))))
      "Base case.")

  (is (expand-macro '(let* () 1))
      1
      "Base case - no bindings.")

  (is (expand-macro '(let* ((x 1))))
      '(let ((x 1))
         (progn))
      "Base case - no body."))


(finalize)
