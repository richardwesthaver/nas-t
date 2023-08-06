;;; infra/web/tests.lisp --- API tests for nas-t.net landing page
(require 'sb-rt)
(require 'sb-bsd-sockets)
(defpackage :nas-t.infra.web.tests
  (:use :cl :sb-bsd-sockets :sb-rt)
  (:export :run-tests))
(in-package :nas-t.infra.web.tests)
(rem-all-tests)
(deftest client.index "OK" "OK")
(deftest client.error "OK" "OK")
(deftest server.user "OK" "OK")
(deftest server.error "OK" "OK")
(defmacro run-tests (&optional out) `(do-tests ,@out))
