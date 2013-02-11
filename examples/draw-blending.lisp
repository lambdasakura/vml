;;;;
;;;; ブレンディングのサンプル
;;;;
(defpackage #:vml-blend-sample
  (:use #:cl #:cl-user :kmrcl :vml-types :vml-graphics :vml-fonts))

(cl-annot:enable-annot-syntax)
(in-package #:vml-blend-sample)

;;; ゲームの更新関数
(defun game-main () 
  ;; (vml-graphics:draw-box 0   0 300 200 :color (vml-types:color :r 255 :g 255 :b 255))
  
  (draw-font "通常描画(Normal)" (point :x 0 :y 0) (color :r 255 :g 255 :b 255))
  (draw-box (rect :x 0   :y 20 :w 100 :h 100) (color :r 255 :g 0   :b 0))
  (draw-box (rect :x 100 :y 20 :w 100 :h 100) (color :r 0   :g 255 :b 0))
  (draw-box (rect :x 200 :y 20 :w 100 :h 100) (color :r 0   :g 0   :b 255))

  (draw-font "アルファ合成(Alpha)" (point :x 0 :y 130) (color :r 255 :g 255 :b 255))
  (draw-box (rect :x 0   :y 150 :w 100 :h 100) (color :r 255 :g 0   :b 0   :a 128))
  (draw-box (rect :x 50  :y 150 :w 150 :h 100) (color :r 0   :g 255 :b 0   :a 128))
  (draw-box (rect :x 150 :y 150 :w 150 :h 100) (color :r 0   :g 0   :b 255 :a 128))
 
  (draw-font "加算合成(Additive)" (point :x 0 :y 260) (color :r 255 :g 255 :b 255))
  (gl:blend-func :src-alpha :one)  
  ;; (vml-graphics:draw-box 0   240 300 100 :r 255 :g 255 :b 255 :alpha 128)
  ;; (vml-graphics:draw-box 0   240 100 100 :r 255 :g 0 :b 0 :alpha 128)
  ;; (vml-graphics:draw-box 50  240 150 100 :r 0 :g 255 :b 0 :alpha 128)
  ;; (vml-graphics:draw-box 150 240 150 100 :r 0 :g 0 :b 255 :alpha 128)
  (gl:blend-func :src-alpha :one-minus-src-alpha))

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
(defun blend-sample ()
  (let ((vml (make-instance 'vml-system:vml-system 
			    :game-main #'game-main
			    :game-quit #'game-quit
			    :tex-reload #'reload-textures
			    :game-init #'game-init)))
    ;;; ゲーム実行
    (vml-system:game-start vml)))
