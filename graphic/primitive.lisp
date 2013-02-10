(in-package :cl-user)
(defpackage vml-graphics
  (:nicknames :vml-graphics)
  (:use :cl
	:cl-annot
	:cl-annot.class
	:cl-annot.doc
	))
(in-package :vml-graphics)
(cl-annot:enable-annot-syntax)

@export
(defun draw-box (x y width height &key (r 0.0) (g 0.0) (b 0.0)
				    (color sdl:*black*) (alpha 255.0))
  (gl:color r g b (/ alpha 255.0))
  (gl:disable :texture-2d)
  (gl:with-primitive :polygon
    (gl:vertex x y 1)
    (gl:vertex x (+ y height) 1)
    (gl:vertex (+ x width) (+ y height) 1)
    (gl:vertex (+ x width) y 1))
  (gl:enable :texture-2d))

@export
(defun draw-rectangle (x y width height &key (line-width 1.0) (r 0.0) (g 0.0) (b 0.0)
				    (color sdl:*black*) (alpha 255.0))
  (gl:color r g b (/ alpha 255.0))
  (gl:disable :texture-2d)
  ;; (gl:line-width width)
  (gl:line-width line-width)
  (gl:with-primitive :line-loop
    (gl:vertex x y 1)
    (gl:vertex x (+ y height) 1)
    (gl:vertex (+ x width) (+ y height) 1)
    (gl:vertex (+ x width) y 1))
  (gl:color 1.0 1.0 1.0 1.0)
  (gl:enable :texture-2d))

