;;;;------------------------------------------------------------------------;;;;
;;;; keyboard.lisp manipulating keyboard.
;;;;
;;;; Date: 2013.03.25
;;;; Author: lambda_sakura(lambda.sakura@gmail.com)
;;;;
;;;;------------------------------------------------------------------------;;;;

#|

VML-Keyboard
------------------

キーボードの状態を管理する。
key-pressed,key-pushedで現在のキーボードの状態を取得できる。


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

|#

(defpackage #:vml-keyboard
  (:use #:cl #:cl-user :kmrcl)
  (:export :set-key-up
	   :set-key-down
	   :keyboard-init
	   :keyboard-update
	   :key-pressed
	   :key-pushed))

(in-package  #:vml-keyboard)

(defparameter *current-device-status* nil)
(defparameter *keyboard-state* nil)

(defclass keyboard-state ()
  ((current :initform (make-hash-table :test 'equal) :accessor current)
   (prev :initform (make-hash-table :test 'equal) :accessor prev)))

(defun sdl-key-name->vml-key-name (sdl-key-name)
  "SDLのキーネームからVMLのキーネームに変換する"
  (intern (cl-ppcre:regex-replace "SDL" (format nil "~A" sdl-key-name) "VML") :keyword))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-key-up (key)
  "keyのボタン押下状態を変更する"
  (setf (gethash (sdl-key-name->vml-key-name key) *current-device-status*) nil))

(defun set-key-down (key)
  "keyのボタン押下状態を変更する"
  (setf (gethash (sdl-key-name->vml-key-name key) *current-device-status*) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun keyboard-init ()
  "キーボードの状態を初期化する"
  (setf *keyboard-state* (make-instance 'keyboard-state))  
  (setf *current-device-status* (make-hash-table :test 'equal)))

(defun keyboard-update ()
  "各フレームごとに呼び出されるキーボード更新関数"
  (setf (prev *keyboard-state*) (alexandria:copy-hash-table (current *keyboard-state*)))
  (setf (current *keyboard-state*) (alexandria:copy-hash-table *current-device-status*)))

(defun key-pressed (key)
  "指定されたbuttonが現在押されているかを判定する"
  (gethash key (current *keyboard-state*)))

(defun key-pushed (key)
  "指定されたbuttonが今押されたかを判定する"
  (and  (eq (gethash key (prev *keyboard-state*)) nil)
	(eq (gethash key (current *keyboard-state*)) t) ))
