;;; pkg.lisp --- NAS-T package definitions

;;; Code:
(defpackage nas-t.util
  (:use :cl :macs :sxp))
(defpackage nas-t.proto
  (:use :cl :nas-t.util))
(defpackage nas-t
  (:use :cl :nas-t.util :nas-t.proto)
  (:export :main))

(in-package :nas-t)
(defun main ()
  (print "hello world"))
