#|
  This file is a part of avm project.
  Copyright (c) 2016 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage avm-test.api.cuda
  (:use :cl
        :prove
        :avm
        :avm.api.cuda))
(in-package :avm-test.api.cuda)


(plan nil)

(subtest "CUDA state"

  (let ((cl-cuda:*sdk-not-found*))      ; fake CUDA not available
    (with-cuda (nil)
      (ok (cuda-state-not-available-p)
          "CUDA is not available.")))

  (with-cuda (nil)
    (ok (cuda-state-not-available-p)
        "CUDA state \"Not available\"."))

  (with-cuda (0)
    (ok (cuda-state-used-p)
        "CUDA state \"Used\".")
    (let ((*use-cuda-p* nil))
      (ok (cuda-state-available-p)
          "CUDA state \"Available\"")))

  )

(finalize)
