# VML(Visual and Multimedeia Library)
======================

VMLは、Common Lispでゲーム開発を行う人向けのライブラリです
 
使い方
------

`
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
`
 
利用しているライブラリ
--------

* lispbuilder-sdl
* lispbuilder-sdl-gfx
* lispbuilder-sdl-image
* lispbuilder-sdl-ttf
* lispbuilder-sdl-mixer
* cl-ppcre
* cl-annot
* cl-test-more
* cl-opengl
* cl-store
* cl-fad
* kmrcl
 
Lisence
----------
Copyright &copy; 2013 lambda_sakura
Distributed under the [MIT License][mit].

[MIT]: http://www.opensource.org/licenses/mit-license.php

