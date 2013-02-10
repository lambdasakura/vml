(in-package  #:vml-keyboard)
(defpackage #:vml-keyboard
  (:use #:cl #:cl-user :kmrcl))
(cl-annot:enable-annot-syntax)
(defparameter *keyboard* (make-hash-table))

;; key1 key2 key3 key4 key5 key6 key7 key8 key9 key0
;; key-a key-b key-c key-d key-e key-f key-g key-h key-i
;; key-j key-k key-l key-m key-n key-o key-p key-q key-r
;; key-s key-t key-u key-v key-w key-x key-y key-z 
;; key-colon key-semicolon key-less key-equals key-greater
;; key-question key-at key-leftbracket key-backslash
;; key-right-bracket key-caret key-underline key-backquote
;; key-backspace key-tab key-clear key-return key-pause
;; key-escape key-exclaim key-quotedbl key-hash
;; key-dollar key-ampersand key-quote key-leftparen 
;; key-rightparen key-asterisk key-plus key-comma
;; key-minus key-period key-slash key-delete
;; key-up key-down key-left key-right)

@export
(defclass keyboard-manager ()
  ((state :initform (make-hash-table :test 'equal)
	  :accessor state)))

@export
(defun init-keyboard ()
  (setf *keyboard* 
	(make-hash-table)))

@export
(defun up-key (key)
  (setf (gethash key *keyboard*) nil))

@export
(defun down-key (key)
  (setf (gethash key *keyboard*) t))

@export
(defun keyboard-state (button)
  (gethash button *keyboard*))
