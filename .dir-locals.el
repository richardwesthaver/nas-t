;;; .dir-locals.el --- Emacs project-local configuration

;; TODO 2023-09-03: need a wrapper to start emacs with dir-locals enabled automatically

;;; Code:
((nil . ((indent-tabs-mode)
	 (fill-column . 88)
	 (project-vc-ignores . ("./scratch"))
         (compile-command . "make ")))  ; FIX 2023-09-03: should be function

 (makefile-mode . ((indent-tabs-mode . t)
                   (outline-regexp . "###+")))

 (org-mode . ((org-edit-src-content-indentation 0)))
 (rust-mode . ((rust-indent-offset . 2)))
 (sh-mode . ((sh-indentation . 2)))
 (sh-script-mode . ((sh-indentation . 2))))
