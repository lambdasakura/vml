;;;;
;;;; フォント描画のサンプル
;;;;
(defpackage #:vml-input-sample
  (:use #:cl #:cl-user :kmrcl :vml-types))

(cl-annot:enable-annot-syntax)
(in-package #:vml-input-sample)

(defparameter all-key '(:key-1 :key-2 :key-3 :key-4 :key-5 :key-6 :key-7 :key-8 :key-9 :key-0
			:key-a :key-b :key-c :key-d :key-e :key-f :key-g :key-h :key-i
			:key-j :key-k :key-l :key-m :key-n :key-o :key-p :key-q :key-r
			:key-s :key-t :key-u :key-v :key-w :key-x :key-y :key-z 
			:key-colon :key-semicolon :key-less :key-equals :key-greater
			:key-question :key-at :key-leftbracket :key-backslash
			:key-right-bracket :key-caret :key-underline :key-backquote
			:key-backspace :key-tab :key-clear :key-return :key-pause
			:key-escape :key-exclaim :key-quotedbl :key-hash
			:key-dollar :key-ampersand :key-quote :key-leftparen 
			:key-rightparen :key-asterisk :key-plus :key-comma
			:key-minus :key-period :key-slash :key-delete
			:key-up :key-down :key-left :key-right))

;;; ゲームの更新関数
(defun game-main () 
  (vml-fonts:draw-font (format nil "Press  Keyboard  Button")
		       (point :x 220 :y 10)
		       (color :r 255 :g 255 :b 255)
		       :type :solid)
  (loop for key in all-key 
     for index = 0 then (+ index 15)
     do 
       (vml-fonts:draw-font (format nil "~A" key)
  			    (point :x (+ 1 (* (floor index 450) 200)) :y (+ 1 (+ 30 (mod index 450))))
  			    (color :r 120 :g 120 :b 120)
  			    :type :solid)
       (vml-fonts:draw-font (format nil "~A" 
     				    (vml-keyboard:keyboard-pushed 
				     (intern (format nil "VML-~A" key) :keyword)))
			    (point :x (+ 130 (* (floor index 450) 200)) :y (+ 30 (mod index 450)))
			    (color :r 255 :g 255 :b 255)
			    :type :solid)
     (vml-fonts:draw-font (format nil "~A"
				  (vml-keyboard:keyboard-pressed 
				     (intern (format nil "VML-~A" key) :keyword)))
			  (point :x (+ 150 (* (floor index 450) 200)) :y (+ 30 (mod index 450)))
			  (color :r 255 :g 255 :b 0)
			  :type :blend)
       ))

;;; ゲームの終了関数
;;; ゲーム終了時にVMLから自動的に呼ばれる
(defun game-quit () )

;;;
;;; テクスチャのリロード関数
;;; テクスチャのリロードが必要になったときにVMLから自動的に呼ばれる
(defun reload-textures () )

(defun load-japanese-font ()
  (unless (sdl:initialise-default-font 
	   (make-instance 'sdl:ttf-font-definition
			  :size 14
			  :filename 
			  (sdl:create-path
			   (sdl:create-path "mika-p.ttf" "Resource/") (truename "."))))
    (error "FONT-EXAMPLE: Cannot initialize the default font.")))

;;; ゲームの初期化関数
;;; ゲームの起動時に自動的に呼ばれる
(defun game-init ()
  (vml-fonts:init-fonts)
  (load-japanese-font))


;;;
;;; ゲームのエントリポイント
;;; 
@export
(defun keyboard-sample ()
  (let ((vml (make-instance 'vml-system:vml-system 
			    :game-main #'game-main
			    :game-quit #'game-quit
			    :tex-reload #'reload-textures
			    :game-init #'game-init)))
    ;;; ゲーム実行
    (vml-system:game-start vml)))
