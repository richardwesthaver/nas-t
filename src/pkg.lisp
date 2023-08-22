;;; pkg.lisp --- NAS-T package definitions
(defpackage nas-t.utils
  (:use :cl :macs :sxp))
(defpackage nas-t.proto
  (:use :cl :nas-t.utils))
(defpackage nas-t
  (:use :cl :nas-t.utils :nas-t.proto)
  (:export :main))

(in-package :nas-t)
(defun main ()
  (print "hello world"))
