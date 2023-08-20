;;; src/fs/btrfs/tests.lisp --- BTRFS common-lisp tests
(defpackage btrfs.tests
  (:use :cl :sb-rt :btrfs)
  (:export :run-all-tests))
(in-package :btrfs.tests)
(defun run-all-tests ())
