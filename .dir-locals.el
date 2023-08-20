;; need a wrapper to start emacs with dir-locals enabled automatically
((nil . ((indent-tabs-mode)
	 (fill-column . 88)
	 (project-vc-ignores . ("./scratch"))
         (compile-command . "make ")))
 (org-mode . ((org-edit-src-content-indentation 0)))
 (rust-mode . ((rust-indent-offset . 2)))
 (sh-mode . ((sh-indentation . 2)))
 (sh-script-mode . ((sh-indentation . 2))))
