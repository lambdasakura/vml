

(defun game-main () )
(defun game-quit () )
(defun reload-textures () )
(defun game-init () )

(defun main ()
  (let ((vml (make-instance 'vml-system:vml-system 
			    :game-main #'game-main
			    :game-quit #'game-quit
			    :tex-reload #'reload-textures
			    :game-init #'game-init)))
    (vml-system:game-start vml)))
