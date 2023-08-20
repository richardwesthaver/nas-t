;;; nas-t.asd --- NAS-T SYSTEMS
(defsystem "nas-t"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/startup/nas-t/issues"
  :depends-on (:cl-ppcre :macs :sxp)
  :in-order-to ((test-op (test-op "nas-t/tests")))
  :components ((:module "src"
		:components ((:file "pkg")
			     (:module "proto"
                              :components ((:file "crypto")
                                           (:file "endpoint")
                                           (:file "message")
                                           (:file "query")
                                           (:file "token")
                                           (:file "transport")))
                             (:module "client")
                             (:module "util"
                                      :components ((:file "macs")))))))

(defsystem "nas-t/infra"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/startup/nas-t/issues"
  :depends-on (:nas-t)
  :components ((:module "infra"
		:components ((:file "pkg")
			     (:file "web/tests")))))

(defsystem "nas-t/docs"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/startup/nas-t/issues"
  :depends-on(:nas-t :organ)
  :components ((:module "docs"
		:components ((:file "pkg")))))

(defsystem "nas-t/tests"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/startup/nas-t/issues"
  :depends-on (:nas-t :sb-rt)
  :components ((:module "tests"
		:components ((:file "pkg"))))
  :perform (test-op (op c) (uiop:symbol-call '#:nas-t.tests '#:run-all-tests)))
