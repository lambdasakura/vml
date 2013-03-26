;;;;------------------------------------------------------------------------;;;;
;;;; keyboard.lisp manipulating keyboard.
;;;;
;;;; Date: 2013.03.25
;;;; Author: lambda_sakura(lambda.sakura@gmail.com)
;;;;
;;;;------------------------------------------------------------------------;;;;

(defpackage #:vml-keyboard
  (:use #:cl #:cl-user :kmrcl))
(cl-annot:enable-annot-syntax)
(in-package  #:vml-keyboard)

#|

 keyboard wrapper 
------------------

|#
;; (defconstant keyboard-list 
;;   '(:vml-key1 :vml-key2 :vml-key3 :vml-key4 :vml-key5 :vml-key6 :vml-key7 :vml-key8 :vml-key9 :vml-key0
;;     :vml-key-a :vml-key-b :vml-key-c :vml-key-d :vml-key-e :vml-key-f :vml-key-g :vml-key-h :vml-key-i
;;     :vml-key-j :vml-key-k :vml-key-l :vml-key-m :vml-key-n :vml-key-o :vml-key-p :vml-key-q :vml-key-r
;;     :vml-key-s :vml-key-t :vml-key-u :vml-key-v :vml-key-w :vml-key-x :vml-key-y :vml-key-z
;;     :vml-key-colon :vml-key-semicolon :vml-key-less :vml-key-equals :vml-key-greater
;;     :vml-key-question :vml-key-at :vml-key-leftbracket :vml-key-backslash
;;     :vml-key-right-bracket :vml-key-caret :vml-key-underline :vml-key-backquote
;;     :vml-key-backspace :vml-key-tab :vml-key-clear :vml-key-return :vml-key-pause
;;     :vml-key-escape :vml-key-exclaim :vml-key-quotedbl :vml-key-hash
;;     :vml-key-dollar :vml-key-ampersand :vml-key-quote :vml-key-leftparen
;;     :vml-key-rightparen :vml-key-asterisk :vml-key-plus :vml-key-comma
;;     :vml-key-minus :vml-key-period :vml-key-slash :vml-key-delete
;;     :vml-key-up :vml-key-down :vml-key-left :vml-key-right))

#|
current-device-status
----------------------

to management physical keyboard state.
sdl-main-loop(vml:game-start) update this table.

up-key & down-key
-----------------

these functions are called at key event.
update device state.
|#
(defparameter *current-device-status* 
  (make-hash-table :test 'equal))

@export
(defun init-current-device-status ()
  (setf *current-device-status* (make-hash-table :test 'equal)))

@export
(defun up-key (key)
  (setf (gethash (sdl-key-name->vml-key-name key) 
		 *current-device-status*)
	nil))

@export
(defun down-key (key)
  (setf (gethash (sdl-key-name->vml-key-name key)
		 *current-device-status*)
	t))

(defun sdl-key-name->vml-key-name (sdl-key-name)
  (intern (cl-ppcre:regex-replace
	   "SDL"
	   (format nil "~A" sdl-key-name)
	   "VML") :keyword))


(defclass keyboard-state ()
  ((current :initform (make-hash-table :test 'equal)
	    :accessor current)
   (prev :initform (make-hash-table :test 'equal)
	 :accessor prev)))

(defparameter *keyboard-state* 
  (make-instance 'keyboard-state))

@export 
(defun keyboard-update ()
  (setf (prev *keyboard-state*)
	(alexandria:copy-hash-table 
	 (current *keyboard-state*)))
  (setf (current *keyboard-state*)
	(alexandria:copy-hash-table 
	 *current-device-status*)))

@export
(defun keyboard-pressed (button)
  (gethash button (current *keyboard-state*)))

@export
(defun keyboard-pushed (button)
  (and (eq (gethash button (prev *keyboard-state*)) nil)
       (eq (gethash button (current *keyboard-state*)) t)))

@export
(defun init-keyboard ()
  (setf *keyboard-state* 
	(make-instance 'keyboard-state))  
  (setf *current-device-status* 
	(make-hash-table :test 'equal)))
