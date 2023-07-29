;; nas-t.asd --- NAS-T SYSTEMS
(defsystem "comp.nas-t"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/startup/nas-t/issues"
  :depends-on (:sxp :organ :log4cl)
  :in-order-to ((test-op (test-op "comp.nas-t.tests")))
  :components ((:module "src")))

(defsystem "comp-nas-t.infra"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/startup/nas-t/issues"
  :depends-on (:comp-nas-t)
  :components ((:module "infra")))

(defsystem "com.nas-t.docs"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/startup/nas-t/issues"
  :depends-on(:comp.nas-t)
  :components ((:module "docs")))

(defsystem "comp.nas-t.tests"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/startup/nas-t/issues"
  :depends-on (:comp-nas-t)
  :components ((:module "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:nas-t-tests '#:run-tests)))
