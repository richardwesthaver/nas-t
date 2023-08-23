(in-package :nas-t.infra.linux)

(defmacro with-mountinfo (obj &body body)
  "Anaphorically bind the slots of OBJ for the extent of BODY. OBJ is an instance of
`mountinfo'."
  ;; (with-gensyms)
  `(let* ((mountinfo ,obj)
          (dev (slot-value mountinfo :dev))
          (fstype (slot-value mountinfo :fstype))
          (opts (slot-value mountinfo :opts))
          (dump (slot-value mountinfo :dump))
          (fsck (slot-value mountinfo :fsck)))
     (ignore mountinfo dev fstype opts dump fsck)
     ,@body))

(defclass devid ()
  ((idtype :initarg :idtype :initform 'label :type symbol)
   (device :initarg :device :type string))
  (:documentation "A storage device identifier"))

(defclass mountinfo ()
  ((dev :initarg :dev :type devid)
   (fstype :initarg :fstype :type string)
   (opts :initarg :opts :type list)
   (dump :initarg :dump :type unsigned-byte)
   (fsck :initarg :fsck :type unsigned-byte))
  (:documentation "A block device mountpoint description."))

(defclass fstab ()
  ((entries :initarg :entries :type (vector moutinfo)))
  (:documentation "fstab class. impl derived from
[[https://github.com/util-linux/util-linux/blob/master/libmount/src/tab_parse.c]]
and
[[https://github.com/util-linux/util-linux/blob/master/libmount/src/mountP.h]]."))
