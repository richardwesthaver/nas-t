;;; src/fs/btrfs/tests.lisp --- BTRFS common-lisp tests

;;; Code:
(defpackage btrfs.tests
  (:use :cl :macs.rt :btrfs)
  (:export :run-all-tests))
(in-package :btrfs.tests)
(defun run-all-tests ())
