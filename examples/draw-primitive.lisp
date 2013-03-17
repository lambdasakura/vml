;;;;
;;;; VMLを初期化してウィンドウを表示するだけのサンプル
;;;;
(defpackage #:vml-draw-primitive-sample
  (:use #:cl #:cl-user :kmrcl :vml-types))

(cl-annot:enable-annot-syntax)
(in-package #:vml-draw-primitive-sample)

;;; ゲームの更新関数
(defun game-main () 
  (vml-fonts:draw-font "Point"
		       (point :x 0 :y 0)
		       (color :r 255 :g 255 :b 255)
		       :type :solid)
  (vml-graphics:draw-point (vml-types:point :x 1 :y 2)
			   (vml-types:color :r 255 :g 255 :b 255))

  (vml-graphics:draw-line (vml-types:point :x 10 :y 20)
			  (vml-types:point :x 30 :y 40)
			  (vml-types:color :r 255 :g 255 :b 255))
  (vml-graphics:draw-triangle (vml-types:point :x 10 :y 20)
			      (vml-types:point :x 30 :y 40)
			      (vml-types:point :x 40 :y 30)
			      (vml-types:color :r 255 :g 255 :b 255)))

;;; ゲームの終了関数
;;; ゲーム終了時にVMLから自動的に呼ばれる
(defun game-quit () )

;;;
;;; テクスチャのリロード関数
;;; テクスチャのリロードが必要になったときにVMLから自動的に呼ばれる
(defun reload-textures () )

(defun convert-relative-to-absolute-path (filename)
  (sdl:create-path
   (sdl:create-path filename) (truename ".")))

(defun load-japanese-font ()
  (let ((abs-path (convert-relative-to-absolute-path
				"Resource/mika-p.ttf")))
    (cond ((cl-fad:file-exists-p abs-path)
	   (sdl:initialise-default-font 
	    (make-instance 'sdl:ttf-font-definition
			   :size 20
			   :filename 
			   abs-path)))
	  (t (error "FONT-EXAMPLE: Cannot initialize the default font.")))))

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
