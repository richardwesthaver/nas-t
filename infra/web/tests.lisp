;;; infra/web/tests.lisp --- API tests for nas-t.net landing page
(require :sb-rt)
(require :sb-bsd-sockets)
(defpackage :nas-t.infra.web.tests
  (:use :cl :sb-rt :sb-bsd-sockets))

(in-package :nas-t.infra.web.tests)

(deftest index (print "ERR"))
