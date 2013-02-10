;;;;
;;;; VMLを初期化してウィンドウを表示するだけのサンプル
;;;;
(defpackage #:vml-draw-primitive-sample
  (:use #:cl #:cl-user :kmrcl))

(cl-annot:enable-annot-syntax)
(in-package #:vml-draw-primitive-sample)

;;; ゲームの更新関数
(defun game-main () 
  (vml-graphics:draw-box 0   0 300 200 :r 255 :g 255 :b 255)
  
  (vml-fonts:draw-font "通常描画" 70 410 :color sdl:*red*)
  (vml-graphics:draw-box 0   0 100 100 :r 255)
  (vml-graphics:draw-box 100 0 100 100 :g 255)
  (vml-graphics:draw-box 200 0 100 100 :b 255)

  (vml-fonts:draw-font "アルファ合成" 70 420 :color sdl:*black*)
  (vml-graphics:draw-box 0   100 100 100 :r 255 :alpha 128)
  (vml-graphics:draw-box 50 100 150 100 :g 255 :alpha 128)
  (vml-graphics:draw-box 150 100 150 100 :b 255 :alpha 128)
 
  (vml-fonts:draw-font "加算合成" 70 420 :color sdl:*white*)
  (gl:blend-func :src-alpha :one)  
  (vml-graphics:draw-box 0   240 300 100 :r 255 :g 255 :b 255 :alpha 128)
  (vml-graphics:draw-box 0   240 100 100 :r 255 :g 0 :b 0 :alpha 128)
  (vml-graphics:draw-box 50  240 150 100 :r 0 :g 255 :b 0 :alpha 128)
  (vml-graphics:draw-box 150 240 150 100 :r 0 :g 0 :b 255 :alpha 128)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  )

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
			  :size 20
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
(defun primitive-sample ()
  (let ((vml (make-instance 'vml-system:vml-system 
			    :game-main #'game-main
			    :game-quit #'game-quit
			    :tex-reload #'reload-textures
			    :game-init #'game-init)))
    ;;; ゲーム実行
    (vml-system:game-start vml)))
