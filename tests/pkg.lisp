(require 'sb-rt)
(require 'sb-bsd-sockets)
(defpackage nas-t.tests
  (:use :cl :sb-bsd-sockets :sb-rt)
  (:export #:run-tests
	   #:run-all-tests
	   ;; reexports from `sb-rt'
	   #:*do-tests-when-defined* #:*test* #:continue-testing
           #:deftest #:do-test #:do-tests #:get-test #:pending-tests
           #:rem-all-tests #:rem-test))
(in-package :nas-t.tests)
(defmacro run-tests (&optional out) `(do-tests ,@out))
(defun run-all-tests () (run-tests))
