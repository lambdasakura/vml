;;;;------------------------------------------------------------------------;;;;
;;;; joystick.lisp manipulating joystick(s)
;;;;
;;;; Date: 2013.03.25
;;;; Author: lambda_sakura(lambda.sakura@gmail.com)
;;;;
;;;;------------------------------------------------------------------------;;;;

(defpackage #:vml-joystick
  (:nicknames :joystick)
  (:use #:cl #:cl-user :kmrcl))
(in-package :vml-joystick)
(cl-annot:enable-annot-syntax)

@export
(defparameter *joystick-manager* nil)

@export
(defun init-joystick ()
  (setf *joystick-manager*
	(make-instance 'joystick-manager)))

@export
(defun handle-cursor-event (axis value index)
  (update-cursor *joystick-manager* axis value index))
  
@export
(defun handle-button-press-event (button index)
  (button-press *joystick-manager* button index))

@export
(defun handle-button-release-event (button index)
  (button-release *joystick-manager* button index))

@export
(defun joystick-update ()
  (let ((joystick (aref (joystick *joystick-manager*) 0)))
    (when (not (= 0 joystick))
      (loop for key in '(:up :down :left :right
			 :button0 :button1 :button2 :button3 :button4
			 :button5 :button6 :button7 :button8
			 :button9 :button10 :button11) do
	   (update-prev-and-current (joystick-state joystick) key)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; button current state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@export
(defun joystick-button-state (button
			      &optional (index 0))
  (cond ((> (num-joysticks *joystick-manager*) 0)
	 (gethash button (state (joystick-state (aref (joystick *joystick-manager*) index)))))
	(t
	 nil)))


@export
(defun joystick-button-pushed (button
				   &optional (index 0))
  (cond ((> (num-joysticks *joystick-manager*) 0)
	 (gethash button (pushed (joystick-state (aref (joystick *joystick-manager*) index)))))
	(t
	 nil)))

@export
(defun joystick-button-press (button
				   &optional (index 0))
  (cond ((> (num-joysticks *joystick-manager*) 0)
	 (gethash button (press (joystick-state (aref (joystick *joystick-manager*) index)))))
	(t
	 nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ジョイスティックのデバイスに依存する情報
;; SDLに依存している部分
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass joystick-device-info ()
 ((fp :initform nil :accessor fp :initarg :fp)
   (index :initform nil :accessor index :initarg :index)))

(defmethod initialize-instance :after ((self joystick-device-info) &rest arg)
  (declare (ignore arg))
  (setf (fp self) (sdl-cffi::sdl-joystick-open (index self)))
  (format t "~A~%" (sdl-cffi::sdl-joystick-name (index self)))
  (format t "~A~%" (fp self))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ジョイスティックがいまどうなっているかを表す
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass joystick-state ()
  ((state :initform (make-hash-table :test 'equal) :accessor state)
   (current :initform (make-hash-table :test 'equal) :accessor current)
   (prev :initform (make-hash-table :test 'equal) :accessor prev)
   (pushed :initform (make-hash-table :test 'equal) :accessor pushed)
   (press :initform (make-hash-table :test 'equal) :accessor press)))

(defmethod update-joystick-state ((self joystick-state) button next-state)
  (let ((state (state self)))
    (setf (gethash button state) next-state)))

(defmethod update-prev-and-current ((self joystick-state) key)
  (let ((state (state self))
	(pushed (pushed self))
	(press (press self))
	(prev (prev self))
	(current (current self)))
    
    (setf (gethash key prev) (gethash key current))
    (setf (gethash key current) (gethash key state))

    (setf (gethash key pushed) 
	  (and (eq nil (gethash key prev))
	       (eq t (gethash key current))))
    (setf (gethash key press) (gethash key current))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; joystick自身
;; デバイスに関する情報、ボタンの状態を保持
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass joystick ()
  ((device-info :initform nil :accessor device-info :initarg :device-info)
   (joystick-state :initform (make-instance 'joystick-state) :accessor joystick-state)))

(defmethod update-joystick-axis ((self joystick) axis value)
  (cond ((= axis 0)
	 (update-joystick-vertical self value))
	((= axis 1)
	 (update-joystick-horizontal self value))))

(defmethod update-joystick-vertical ((self joystick) value)
  (let ((state (joystick-state self)))
    (update-joystick-state state :right nil)
    (update-joystick-state state :left nil)
    (cond ((>= value 512)
	   (update-joystick-state state :right t))
	  ((<= value -512)
	   (update-joystick-state state :left t)))))
	  
(defmethod update-joystick-horizontal ((self joystick) value)
  (let ((state (joystick-state self)))
    (update-joystick-state state :up nil)
    (update-joystick-state state :down nil)
    (cond ((>= value 512)
	   (update-joystick-state state :down t))
	  ((<= value -512)
	   (update-joystick-state state :up t)))))

@export
(defun close-joystick (joystick)
  (when (> (sdl-cffi::sdl-joystick-opened (joystick-index joystick)) 0)
    (sdl-cffi::sdl-joystick-close (joystick-fp joystick))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; joystick manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export 
(defclass joystick-manager ()
  ((num-joysticks :initform 0 :accessor num-joysticks)
   (index :initform 0 :accessor index :initarg :index)
   (joystick :initform (make-array 4) :accessor joystick :initarg :joystick)))

(defmethod initialize-instance :after ((self joystick-manager) &rest arg)  
  (declare (ignore arg))
  (sdl-cffi::sdl-joystick-event-state sdl-cffi::sdl-enable)
  (setf (index self) 0)
  (let* ((num-joysticks (sdl:num-joysticks))
	 (device-info (make-instance
		       'joystick-device-info :index (index self))))
    (setf (num-joysticks self) num-joysticks)
    (format t "~A~%" (sdl:num-joysticks))
    (cond ((> num-joysticks 0)
	   (setf (aref (joystick self) (index self))
		 (make-instance 'joystick :device-info device-info)))
	  (t 
	   nil))))

  
@export
(defmethod update-cursor ((self joystick-manager) AXIS VALUE &optional (index 0))
  (let ((joystick (aref (joystick self) index)))
    (when joystick
      (update-joystick-axis joystick axis value))))

@export
(defmethod button-press ((self joystick-manager) button &optional (index 0))
  (let* ((joystick (aref (joystick self) index))
	 (state (joystick-state joystick)))
    (when joystick
      (case button
	(0 (update-joystick-state state :button0 t))
	(1 (update-joystick-state state :button1 t))
	(2 (update-joystick-state state :button2 t))
	(3 (update-joystick-state state :button3 t))
	(4 (update-joystick-state state :button4 t))
	(5 (update-joystick-state state :button5 t))
	(6 (update-joystick-state state :button6 t))
	(7 (update-joystick-state state :button7 t))
	(8 (update-joystick-state state :button8 t))
	(9 (update-joystick-state state :button9 t))
	(10 (update-joystick-state state :button10 t))
	(11 (update-joystick-state state :button11 t))
	))))

@export
(defmethod button-release ((self joystick-manager) button &optional (index 0))
  (let* ((joystick (aref (joystick self) index))
	 (state (joystick-state joystick)))
    (when joystick
      (case button
	(0 (update-joystick-state state :button0 nil))
	(1 (update-joystick-state state :button1 nil))
	(2 (update-joystick-state state :button2 nil))
	(3 (update-joystick-state state :button3 nil))
	(4 (update-joystick-state state :button4 nil))
	(5 (update-joystick-state state :button5 nil))
	(6 (update-joystick-state state :button6 nil))
	(7 (update-joystick-state state :button7 nil))
	(8 (update-joystick-state state :button8 nil))
	(9 (update-joystick-state state :button9 nil))
	(10 (update-joystick-state state :button10 nil))
	(11 (update-joystick-state state :button11 nil))
	))))


