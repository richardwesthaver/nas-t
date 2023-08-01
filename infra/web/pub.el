;;; publish.el --- publish nas-t content -*- lexical-binding: t; -*-

;; Copyright (C) 2023  ellis

;; Author: ellis <ellis@rwest.io>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'rw-macs)
(require 'ox-publish)
(defvar nas-t-web-dir default-directory)

(defvar nas-t-web-project
  (list "nas-t.net"
	:base-directory nas-t-web-dir
	:publishing-directory (path-join nas-t-web-dir "ui")
	:exclude "readme.org"
	:base-extension "org"
	:recursive nil
	:htmlized-source t
	:org-publish-use-timestamps-flag nil))

(defun nas-t-web-publish (&optional &rest files)
  "Publish the contents of `nas-t-web-content-dir' or FILES if provided."
  (interactive)
  (let ((files (or files (directory-files nas-t-web-dir t "^[^.].*[.]org$"))))
    (message "publishing files for nas-t.net: %S" files)
    (setq org-html-style-default ""
	  org-html-scripts ""
	  org-html-htmlize-output-type 'css
	  org-export-htmlize-output-type 'css
	  org-html-doctype "html5"
	  org-html-html5-fancy t
	  org-html-validation-link nil
	  org-src-fontify-natively t
	  make-backup-files nil
	  debug-on-error t)
    (dolist (f files)
      (org-publish-file f nas-t-web-project))))

(provide 'nas-t.web.publish)
;;; publish.el ends here
