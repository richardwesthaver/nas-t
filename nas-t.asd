;;; nas-t.asd --- NAS-T SYSTEMS

;; Welcome to NAS-T.

;;; Code:
(defsystem "nas-t"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/startup/nas-t/issues"
  :depends-on (:cl-ppcre :macs :sxp)
  :build-operation "program-op"
  :build-pathname "bin/nasd"
  :entry-point "nas-t:main"
  :in-order-to ((test-op (test-op "nas-t/tests")))
  :components ((:module "src"
		:components
                ((:file "pkg")
		 (:module "proto"
                  :components ((:file "crypto")
                               (:file "endpoint")
                               (:file "message")
                               (:file "query")
                               (:file "token")
                               (:file "transport")))
                 (:module "client")
                 (:module "util"
                  :components ((:file "pkg")
                               (:file "macs" :depends-on ("pkg"))
                               (:file "test" :depends-on ("pkg"))))))))
                                                 
(defsystem "nas-t/infra"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/startup/nas-t/issues"
  :depends-on (:nas-t :macs)
  :components ((:module "infra"
		:components ((:file "pkg")
			     (:file "web/tests")))))

(defsystem "nas-t/tests"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/startup/nas-t/issues"
  :depends-on (:nas-t :macs :macs/rt)
  :components ((:module "tests"
		:components ((:file "pkg"))))
  :perform (test-op (op c) (uiop:symbol-call '#:nas-t.tests '#:run-all-tests)))
