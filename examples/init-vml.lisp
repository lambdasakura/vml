;;;;
;;;; VMLを初期化してウィンドウを表示するだけのサンプル
;;;;

;;; ゲームの更新関数
(defun game-main () )

;;; ゲームの終了関数
;;; ゲーム終了時にVMLから自動的に呼ばれる
(defun game-quit () )

;;;
;;; テクスチャのリロード関数
;;; テクスチャのリロードが必要になったときにVMLから自動的に呼ばれる
(defun reload-textures () )

;;; ゲームの初期化関数
;;; ゲームの起動時に自動的に呼ばれる
(defun game-init () )


;;;
;;; ゲームのエントリポイント
;;; 
(defun main ()
  (let ((vml (make-instance 'vml-system:vml-system 
			    :game-main #'game-main
			    :game-quit #'game-quit
			    :tex-reload #'reload-textures
			    :game-init #'game-init)))
    ;;; ゲーム実行
    (vml-system:game-start vml)))

