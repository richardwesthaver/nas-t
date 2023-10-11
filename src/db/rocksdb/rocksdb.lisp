;;; rocksdb.lisp

;;; Commentary:

;; if ur on archlinux and installed rocksdb via AUR you may receive an error from
;; jemalloc: cannot allocate memory in static TLS block:

;; https://github.com/veer66/cl-rocksdb/issues/1

;; for best results, you should compile rocksdb from source - use j0ni's snippet as a
;; starting point.

;; make shared_lib DISABLE_JEMALLOC=1 && 
;; sudo cp librocksdb.so.* /usr/local/lib/ && 
;; sudo cp -rf include/* /usr/local/include/

;; https://github.com/facebook/rocksdb/blob/main/Makefile

;; check /usr/local/include/rocksdb/c.h for the C API header, the source is under
;; db/c.cc

(defpackage #:rocksdb
  (:use #:cl :sb-alien #:macs.alien)
  (:import-from :sb-ext :string-to-octets :octets-to-string)
  (:export
   :load-rocksdb
   ;; LRU CACHE
   #:create-lru-cache
   ;; BLOCK-BASED OPTIONS
   #:create-block-based-options
   #:destroy-block-based-options
   #:set-block-based-options-block-cache
   #:set-block-based-options-cache-index-and-filter-blocks
   ;; OPTIONS
   #:create-options
   #:destroy-options
   #:increase-parallelism
   #:optimize-level-style-compaction
   #:set-create-if-missing
   #:set-block-based-table-factory-options
   #:create-writeoptions
   #:destroy-writeoptions
   #:create-readoptions
   #:destroy-readoptions
   ;; BASIC
   #:open-db
   #:close-db
   #:cancel-all-background-work
   #:put-kv
   #:put-kv-str
   #:get-kv
   #:get-kv-str
   ;; ITERATOR
   #:create-iter
   #:destroy-iter
   #:move-iter-to-first
   #:move-iter-forward
   #:move-iter-backword
   #:valid-iter-p
   #:iter-key
   #:iter-key-str
   #:iter-value
   #:iter-value-str
   #:with-open-db
   #:with-iter))

(in-package :rocksdb)

(defun load-rocksdb () (sb-alien:load-shared-object "librocksdb.so" :dont-save t))

(load-rocksdb)  

;; Foreign Types
(define-alien-type rocksdb (struct rocksdb_t))
(define-alien-type rocksdb-options (struct rocksdb_options_t))
(define-alien-type rocksdb-readoptions (struct rocksdb_readoptions_t))
(define-alien-type rocksdb-writeoptions (struct rocksdb_writeoptions_t))
(define-alien-type rocksdb-compactoptions (struct rocksdb_compactoptions_t))
(define-alien-type rocksdb-block-based-table-options (struct rocksdb_block_based_table_options_t))
(define-alien-type rocksdb-iterator (struct rocksdb_iterator_t))
(define-alien-type rocksdb-column-family-handle (struct rocksdb_column_family_handle_t))
(define-alien-type rocksdb-sstfilewriter (struct rocksdb_sstfilewriter_t))
(define-alien-type errptr (* (* char)))

;; LRU

(define-alien-routine ("rocksdb_cache_create_lru" create-lru-cache) (* rocksdb) (capacity unsigned-int))

;; Block based options
(define-alien-routine "rocksdb_block_based_options_create" (* t))
(define-alien-routine ("rocksdb_block_based_options_destroy" destroy-block-based-options) void (options (* t)))
(define-alien-routine ("rocksdb_block_based_options_set_block_cache" set-block-based-options-block-cache) void (options (* t)) (block-cache (* t)))
(define-alien-routine 
    ("rocksdb_block_based_options_set_cache_index_and_filter_blocks" set-block-based-options-cache-index-and-filter-blocks) 
  void (options (* t)) (val c-string))

;; Options
(define-alien-routine ("rocksdb_options_create" create-options) (* t))
(define-alien-routine ("rocksdb_options_destroy" destroy-options) void (options (* t)))
(define-alien-routine ("rocksdb_options_increase_parallelism" increase-parallelism) void (opt (* t)) (total-threads int))
(define-alien-routine ("rocksdb_options_optimize_level_style_compaction" optimize-level-style-compaction) void (opt (* t)) (memtable_memory_budget (unsigned 4)))
(define-alien-routine ("rocksdb_options_set_create_if_missing" set-create-if-missing) void (opt (* t)) (val boolean))
(define-alien-routine ("rocksdb_options_set_block_based_table_factory" set-block-based-table-factory-options) void (opt (* t)) (table-options (* t)))
(define-alien-routine ("rocksdb_writeoptions_create" create-writeoptions) (* t))
(define-alien-routine ("rocksdb_writeoptions_destroy" destroy-writeoptions) void (opt (* t)))
(define-alien-routine ("rocksdb_readoptions_create" create-readoptions) (* t))
(define-alien-routine ("rocksdb_readoptions_destroy" destroy-readoptions) void (opt (* t)))

;; Basic functions
(define-alien-routine ("rocksdb_open" open-db*) (* rocksdb) (opt (* rocksdb-options)) (name c-string) (errptr (* t)))
(define-alien-routine ("rocksdb_close" close-db) void (db (* rocksdb_t)))
(define-alien-routine ("rocksdb_cancel_all_background_work" cancel-all-background-work) void (db (* rocksdb_t)) (wait boolean))
(define-alien-routine ("rocksdb_put" put*) void (db (* rocksdb)) (options (* rocksdb-writeoptions)) (key (* t)) (keylen unsigned-int) (val (* t)) (vallen unsigned-int) (errptr (* t)))
(define-alien-routine ("rocksdb_get" get*) (* t) (db (* rocksdb)) (options (* rocksdb-readoptions)) (key (* t)) (keylen unsigned-int) (vallen (* t)) (errptr (* t)))

;; Iterator
(define-alien-routine ("rocksdb_create_iterator" create-iter*) (* t) (db (* t)) (opt (* t)))
(define-alien-routine ("rocksdb_iter_destroy" destroy-iter) void (iter (* t)))
(define-alien-routine ("rocksdb_iter_seek_to_first" move-iter-to-first) void (iter (* t)))
(define-alien-routine ("rocksdb_iter_valid" valid-iter-p) boolean (iter (* t)))
(define-alien-routine ("rocksdb_iter_next" move-iter-forward) void (iter (* t)))
(define-alien-routine ("rocksdb_iter_prev" move-iter-backward) void (iter (* t)))
(define-alien-routine ("rocksdb_iter_key" iter-key*) (* t) (iter (* t)) (klen-ptr (* t)))
(define-alien-routine ("rocksdb_iter_value" iter-value*) (* t) (iter (* t)) (vlen-ptr (* t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition unable-to-open-db (error)
  ((db-path :initarg :db-path
            :reader db-path)
   (error-message :initarg :error-message
                  :reader error-message)))

(defmethod print-object ((obj unable-to-open-db) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "error-message=~A" (error-message obj))))

(define-condition unable-to-put-key-value-to-db (error)
  ((db :initarg :db
       :reader db)
   (key :initarg :key
        :reader key)
   (val :initarg :val
        :reader val)
   (error-message :initarg :error-message
                  :reader error-message)))
(define-condition unable-to-get-value-to-db (error)
  ((db :initarg :db
       :reader db)
   (key :initarg :key
        :reader key)
   (error-message :initarg :error-message
                  :reader error-message)))
(sb-alien:get-errno)
(defun open-db (db-path &optional opt)
  (unless opt
    (setq opt (create-options)))
  (with-alien ((errptr (* c-string)))
    (let* ((db-path (if (pathnamep db-path)
                        (namestring db-path)
                        db-path))
           (db (open-db* opt db-path errptr))
           (err errptr))
      (unless (null-alien err)
        (error 'unable-to-open-db
               :db-path db-path
               :error-message err))
      db)))

(defmacro clone-octets-to-foreign (lisp-array foreign-array)
  (let ((i (gensym)))
    `(loop for ,i from 0 below (length ,lisp-array)
           do (setf (deref ,foreign-array ,i)
                    (aref ,lisp-array ,i)))))

(defmacro clone-octets-from-foreign (foreign-array lisp-array len)
  (let ((i (gensym)))
    `(loop for ,i from 0 below ,len
           do (setf (aref ,lisp-array ,i)
                    (deref ,foreign-array ,i)))))

(defun put-kv (db key val &optional opt)
  (unless opt
    (setq opt (create-writeoptions)))
  (with-alien ((errptr (* t))
                         (key* unsigned-char (length key))
                         (val* unsigned-char (length val)))
    (clone-octets-to-foreign key key*)
    (clone-octets-to-foreign val val*)
    (put* db
          opt
          key*
          (length key)
          val*
          (length val)
          errptr)
    (let ((err errptr))
      (unless (null-alien err)
        (error 'unable-to-put-key-value-to-db
               :db db
               :key key
               :val val
               :error-message (sap-alien err c-string))))))

(defun put-kv-str (db key val &optional opt)
  (let ((key-octets (string-to-octets key))
        (val-octets (string-to-octets val)))
    (put-kv db key-octets val-octets opt)))

(defun get-kv (db key &optional opt)
  (unless opt
    (setq opt (create-readoptions)))

  (with-alien ((val-len-ptr unsigned-int)
               (errptr system-area-pointer)
               (key* unsigned-char (length key)))
    (clone-octets-to-foreign key key*)
    ;; (setf (mem-ref errptr :pointer) (null-pointer))
    (let ((val (get* db
                     opt
                     key*
                     (length key)
                     val-len-ptr
                     errptr)))
      (let ((err errptr))
        (unless (null-alien err)
          (error 'unable-to-get-value-to-db
                 :db db
                 :key key
                 :error-message (sap-alien err c-string)))
        
        (unless (null-alien val)
          (let* ((val-len val-len-ptr)
                 (val* (make-array val-len
                                      :element-type '(unsigned-byte 8))))
            (clone-octets-from-foreign val val* val-len)
            val*))))))

(defun get-kv-str (db key &optional opt)
  (let ((key-octets (string-to-octets key)))
    (let ((#1=val-octets (get-kv db key-octets opt)))
      (when #1#
        (octets-to-string #1#)))))

(defun create-iter (db &optional opt)
  (unless opt
    (setq opt (create-readoptions)))
  (create-iter* db opt))

(defun iter-key (iter)
  (with-alien ((klen-ptr unsigned-int 0))
    (let* ((key-ptr (iter-key* iter klen-ptr))
           (klen klen-ptr)
           (key (make-array klen :element-type '(unsigned-byte 8))))
      (clone-octets-from-foreign key-ptr key klen)
      key)))

(defun iter-key-str (iter)
  (let ((#1=key-octets (iter-key iter)))
    (when #1#
      (octets-to-string #1#))))

(defun iter-value (iter)
  (with-alien ((len-ptr unsigned-int 0))
    (let* ((value-ptr (iter-value* iter len-ptr))
           (vlen len-ptr)
           (value* (make-array vlen :element-type '(unsigned-byte 8))))
      (clone-octets-from-foreign value-ptr value* vlen)
      value*)))

(defun iter-value-str (iter)
  (let ((#1=val-octets (iter-value iter)))
    (when #1#
      (octets-to-string #1#))))

(defmacro with-open-db ((db-var db-path &optional opt) &body body)
  `(let ((,db-var (open-db ,db-path ,opt)))
     (unwind-protect (progn ,@body)
       (close-db ,db-var))))

(defmacro with-iter ((iter-var db &optional opt) &body body)
  `(let ((,iter-var (create-iter ,db ,opt)))
     (unwind-protect (progn ,@body)
       (destroy-iter ,iter-var))))
