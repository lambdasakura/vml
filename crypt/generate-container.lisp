(in-package #:cl-user)
(defpackage :vml-data
  (:use :cl
	:cl-annot
	:cl-annot.class
	:cl-annot.doc
	:cl-store
	:cl-fad
	)
  (:nicknames :vml-data))
(in-package :vml-data)
(cl-annot:enable-annot-syntax)

@export
(defun create-database (directory base-dir-len &key (log nil))
  (let ((database (make-hash-table :test 'equal)))
    (cl-fad:walk-directory directory
			   (lambda (x) 
			     (let ((path (subseq (format nil "~A" x) base-dir-len)))
			       (if log
				   (format t "~A~%" path))
			       (setf (gethash path database) 
				     (kmrcl:read-file-to-usb8-array x)))))
    database))

(defparameter *crypt-parameter* #xff)
(defun crypt-binary (stream)
  (let ((as (make-array (length stream) :element-type '(unsigned-byte 8))))
    (loop for i from 0 to (- (length stream) 1) do
	 (setf (aref as i) (logxor (aref stream i) *crypt-parameter*)))
    as))
(defun decrypt-binary (stream)
  (let ((as (make-array (length stream) :element-type '(unsigned-byte 8))))
    (loop for i from 0 to (- (length stream) 1) do
	 (setf (aref as i) (logxor (aref stream i) *crypt-parameter*)))
    as))

(defun crypt-key (stream)
  (let ((as (make-array (length stream) :element-type '(unsigned-byte 8))))
    (loop for i from 0 to (- (length stream) 1) do
	 (setf (aref as i) (logxor (char-code (aref stream i)) *crypt-parameter*)))
    as))

(defun decrypt-key (stream)
  (let ((as (make-array (length stream) :element-type 'base-char)))
    (loop for i from 0 to (- (length stream) 1) do
	 (setf (aref as i) (code-char (logxor (aref stream i) *crypt-parameter*))))
    as))

@export
(defun crypt-data (database)
  (let ((crypted-data (make-hash-table :test 'equal)))
    (loop for key being the hash-keys in database using (hash-value i) do
	 (setf (gethash (crypt-key key) crypted-data) 
	       (crypt-binary i)))
    crypted-data))

@export
(defun decrypt-data (database)
  (let ((decrypted-data (make-hash-table :test 'equal)))
    (loop for key being the hash-keys in database using (hash-value i) do
	 (setf (gethash (decrypt-key key) decrypted-data) 
	       (decrypt-binary i)))
    decrypted-data))

