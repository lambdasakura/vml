;;;;
;;;; フォント描画のサンプル
;;;;
(defpackage #:vml-font-sample
  (:use #:cl #:cl-user :kmrcl :vml-types))

(cl-annot:enable-annot-syntax)
(in-package #:vml-font-sample)

;;; ゲームの更新関数
(defun game-main () 
  (vml-fonts:draw-font "Text printing Test"
		       (point :x 0 :y 0)
		       (color :r 255 :g 255 :b 255)
		       :type :solid)
  (vml-fonts:draw-font "日本語表示のテスト(Japanese Font Test)"
		       (point :x 0 :y 30)
		       (color :r 0 :g 0 :b 255)
		       :type :blend)
  (vml-fonts:draw-font "日本語表示のテスト(Japanese Font Test)"
		       (point :x 0 :y 60)
		       (color :r 0 :g 0 :b 255)
		       :type :solid)
  
  ;; (vml-fonts:draw-font "通常描画(Normal)" 0 200 :color (vml-types:color :r 255 :g 0 :b 0))
  ;; (vml-fonts:draw-font "通常描画(Normal)" 0 400 :color (vml-types:color :r 0 :g 255 :b 0))
  ;; (gl:blend-func :src-alpha :one-minus-src-alpha)
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
(defun font-sample ()
  (let ((vml (make-instance 'vml-system:vml-system 
			    :game-main #'game-main
			    :game-quit #'game-quit
			    :tex-reload #'reload-textures
			    :game-init #'game-init)))
    ;;; ゲーム実行
    (vml-system:game-start vml)))
