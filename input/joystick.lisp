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

(defparameter *joystick-manager* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 現在のジョイスティックの状態を保持するクラス
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass joystick-manager ()
  ((num-joysticks :initform 0 :accessor num-joysticks)
   (joystick :initform nil :accessor joystick :initarg :joystick)))
(defclass joystick ()
  ((fp :initform nil :accessor fp :initarg :fp)
   (state :initform (make-hash-table :test 'equal) :accessor state)
   (current :initform (make-hash-table :test 'equal) :accessor current)
   (prev :initform (make-hash-table :test 'equal) :accessor prev)
   (pushed :initform (make-hash-table :test 'equal) :accessor pushed)
   (press :initform (make-hash-table :test 'equal) :accessor press)))

(defmethod initialize-instance :after ((self joystick) &rest arg)
  (declare (ignore arg))
  (setf (fp self) (sdl-cffi::sdl-joystick-open (index self))))

(defmethod initialize-instance :after ((self joystick-manager) &rest arg)  
  (declare (ignore arg))
  ;;  Joystickのイベントを受け取れるようにSDLの関数をcall
  (sdl-cffi::sdl-joystick-event-state sdl-cffi::sdl-enable)
  (let ((num-joysticks (sdl:num-joysticks))
	;; ジョイスティックの数を登録
	(setf (num-joysticks self) num-joysticks)
	
	;; すべてのジョイスティックを初期化
	(cond ((> num-joysticks 0)
	       (setf (joystick self)
		     (loop for i=0 to num-joysticks
			collect (device-info (make-instance 'joystick-device-info :index i))))
	       (t nil))))))

(defun close-joystick (joystick)
  (when (> (sdl-cffi::sdl-joystick-opened (joystick-index joystick)) 0)
    (sdl-cffi::sdl-joystick-close (joystick-fp joystick))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; System API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun init-joystick ()
  (setf *joystick-manager* (make-instance 'joystick-manager)))

(defun handle-cursor-event (axis value index)
  (update-cursor *joystick-manager* axis value index))

(defun handle-button-press-event (button index)
  (button-press *joystick-manager* button index))

(defun handle-button-release-event (button index)
  (button-release *joystick-manager* button index))

(defmethod update-prev-and-current ((self joystick-state) key)
  (let ((state (state self))
	(pushed (pushed self))
	(press (press self))
	(prev (prev self))
	(current (current self)))
    
    (setf (gethash key prev) (gethash key current))
    (setf (gethash key current) (gethash key state))

    (setf (gethash key pushed) (and (eq nil (gethash key prev)) (eq t (gethash key current))))
    (setf (gethash key press) (gethash key current))))

(defun joystick-update ()
  (let ((joystick (aref (joystick *joystick-manager*) 0)))
    (when (not (= 0 joystick))
      (loop for key in '(:up :down :left :right
			 :button0 :button1 :button2 :button3 :button4
			 :button5 :button6 :button7 :button8
			 :button9 :button10 :button11) do
	   (update-prev-and-current (joystick-state joystick) key)))))

(defmethod update-cursor ((self joystick-manager) AXIS VALUE &optional (index 0))
  (let ((joystick (aref (joystick self) index)))
    (when joystick
      (update-joystick-axis joystick axis value))))
  
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


(defmethod update-joystick-state ((self joystick) button next-state)
  "ボタンの状態を引数の状態に更新する"
  (setf (gethash button (state self)) next-state))

(defmethod button-press ((self joystick-manager) button &optional (index 0))
  "ボタンが押された時にボタンの状態を更新する関数"
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
	(11 (update-joystick-state state :button11 t))))))

(defmethod button-release ((self joystick-manager) button &optional (index 0))
  "ボタンが離された時にボタンの状態を更新する関数"
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
	(11 (update-joystick-state state :button11 nil))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun joystick-button-state (button &optional (index 0))
  (cond ((> (num-joysticks *joystick-manager*) 0)
	 (gethash button (state (aref (joystick *joystick-manager*) index))))
	(t nil)))

(defun joystick-button-pushed (button
				   &optional (index 0))
  (cond ((> (num-joysticks *joystick-manager*) 0)
	 (gethash button (pushed (aref (joystick *joystick-manager*) index))))
	(t nil)))

(defun joystick-button-press (button
				   &optional (index 0))
  (cond ((> (num-joysticks *joystick-manager*) 0)
	 (gethash button (press (aref (joystick *joystick-manager*) index))))
	(t nil)))
