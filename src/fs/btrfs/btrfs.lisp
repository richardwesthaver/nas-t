;;; src/fs/btrfs/btrfs.lisp --- BTRFS common-lisp API
(defpackage btrfs
  (:use :cl :sb-alien)
  (:export
   :btrfs-shared-objects
   :btrfs-lib-path
   :load-btrfs :unload-btrfs
   :load-btrfsutil :unload-btrfsutil
   :define-btrfs-ioctl))

(in-package :btrfs)

(defvar btrfs-shared-objects
  (list '(:btrfs "/usr/lib/libbtrfs.so")
        '(:btrfsutil "/usr/lib/libbtrfsutil.so")))

(defun btrfs-lib-path (lib) (cadr (assoc lib btrfs-shared-objects)))
                
(defmacro when-lib-exists-p ((sym lib) &body body)
  `(let ((,sym ,(btrfs-lib-path lib)))
    (when (uiop:file-exists-p ,sym) ,@body)))

(defun load-btrfs ()
  "Open 'libbtrfs' using `dlopen'. exposing the C API to the current Lisp image."
  (when-lib-exists-p (l :btrfs) (load-shared-object l)))

(defun unload-btrfs ()
  "Close 'libbtrfs' using `dlclose'."
  (unload-shared-object (btrfs-lib-path :btrfs)))

(defun load-btrfsutil ()
  "Open 'libbtrfsutil' using `dlopen'. exposing the C API to the current Lisp image."
  (when-lib-exists-p (l :btrfsutil) (load-shared-object l)))

(defun unload-btrfsutil ()
  "Close 'libbtrfsutil' using `dlclose'."
  (unload-shared-object (btrfs-lib-path :btrfsutil)))

(defmacro define-btrfs-ioctl () "Define a wrapper for IOCTLs exposed by BTRFS.")
