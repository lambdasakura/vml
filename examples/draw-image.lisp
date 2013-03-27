;;;;
;;;; Image Drawing Sample
;;;;
(defpackage #:vml-image-sample
  (:use #:cl #:cl-user :kmrcl :vml-types))

(cl-annot:enable-annot-syntax)
(in-package #:vml-image-sample)

(defparameter *image-id* nil)

;;; ゲームの更新関数
(defun game-main () 
  (vml-image:draw-image *image-id*  0 0 ))


;;; ゲームの終了関数
;;; ゲーム終了時にVMLから自動的に呼ばれる
(defun game-quit () )

;;;
;;; テクスチャのリロード関数
;;; テクスチャのリロードが必要になったときにVMLから自動的に呼ばれる
(defun reload-textures () )

;;; ゲームの初期化関数
;;; ゲームの起動時に自動的に呼ばれる
(defun game-init ()  
  (vml-image:initialize-textures)
  (setf *image-id* 
	(vml-image:load-image-file "Resource/lisplogo_alien_128.png")))


;;;
;;; ゲームのエントリポイント
;;; 
@export
(defun image-sample ()
  (let ((vml (make-instance 'vml-system:vml-system 
			    :game-main #'game-main
			    :game-quit #'game-quit
			    :tex-reload #'reload-textures
			    :game-init #'game-init)))
    ;;; ゲーム実行
    (vml-system:game-start vml)))
