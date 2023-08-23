;;; content.el --- publish nas-t content.org -*- lexical-binding: t; -*-

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
(require 'org)
(require 'ox-publish)
(require 'ox-html)
(require 'citeproc)
(defvar nas-t-content-file (expand-file-name "content.org"))

(defun nas-t-content-export (&optional file)
  "Export all subtrees that are *not* tagged with :noexport: to
separate files.

Subtrees that do not have the :EXPORT_FILE_NAME: property set
are exported to a filename derived from the headline text."
  (interactive)
    (save-excursion
      (with-current-buffer (find-file-noselect (or file nas-t-content-file))
	(goto-char (point-min))
	(goto-char (re-search-forward "^*"))
	(set-mark (line-beginning-position))
	(goto-char (point-max))
	(org-map-entries
       (lambda ()
         (let ((export-file (org-entry-get (point) "EXPORT_FILE_NAME")))
           (unless export-file
             (org-set-property
              "EXPORT_FILE_NAME"
              (replace-regexp-in-string " " "_" (nth 4 (org-heading-components)))))
           (deactivate-mark)
           (org-html-export-to-html nil t nil t)
           (unless export-file (org-delete-property "EXPORT_FILE_NAME"))))
       "-noexport" 'region-start-level))))
