;;; docs/pkg.lisp --- NAS-T docs

;;; Code:
(defpackage nas-t.docs
  (:use :cl :macs.cli :nas-t :organ :sxp))

(defvar *doc-file-directory* #.*load-pathname*)
