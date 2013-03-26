;;;;------------------------------------------------------------------------;;;;
;;;; primitive.lisp drawing primitive.
;;;;
;;;; Date: 2013.03.25
;;;; Author: lambda_sakura(lambda.sakura@gmail.com)
;;;;
;;;;------------------------------------------------------------------------;;;;

(in-package :cl-user)
(defpackage vml-graphics
  (:nicknames :vml-graphics)
  (:use :cl
	:cl-annot
	:cl-annot.class
	:cl-annot.doc
	:vml-types
	))
(in-package :vml-graphics)
(cl-annot:enable-annot-syntax)

@export
(defun draw-point (point1 color &key (fill t)) 
  (let ((x (x point1))
	(y (y point1))
	(r (red color))
	(g (green color))
	(b (blue color))
	(a (if (alpha color) (/ (alpha color) 255) 1.0)))
    (gl:color r g b a)
    (gl:disable :texture-2d)
    (gl:with-primitive :points
      (gl:vertex x y 1))
    (gl:enable :texture-2d)))		
  
@export
(defun draw-line (point1 point2 color &key (fill t))
  (let ((x1 (x point1))
	(y1 (y point1))
	(x2 (x point2))
	(y2 (y point2))
	(r (red color))
	(g (green color))
	(b (blue color))
	(a (if (alpha color) (/ (alpha color) 255) 1.0)))
    (gl:color r g b a)
    (gl:disable :texture-2d)
    (gl:with-primitive :lines
      (gl:vertex x1 y1 1)
      (gl:vertex x2 y2 1))
    (gl:enable :texture-2d)))

@export
(defun draw-triangle (point1 point2 point3 color &key (fill t))
  (let ((x1 (x point1))
	(y1 (y point1))
	(x2 (x point2))
	(y2 (y point2))
	(x3 (x point3))
	(y3 (y point3))
	(r (red color))
	(g (green color))
	(b (blue color))
	(a (if (alpha color) (/ (alpha color) 255) 1.0)))
    (gl:color r g b a)
    (gl:disable :texture-2d)
    (gl:with-primitive :triangles
      (gl:vertex x1 y1 1)
      (gl:vertex x2 y2 1)
      (gl:vertex x3 y3 1))
    (gl:enable :texture-2d)))
		      
(defun draw-rectangle (point1 point2 &key (fill t))
  
  )

(defun draw-circle (center-point radius &key (fill t) )
)


@export
(defun draw-box (rect color)
  (let ((x (x rect))
	(y (y rect))
	(width (w rect))
	(height (h rect))
	(r (red color))
	(g (green color))
	(b (blue color))
	(a (if (alpha color) (/ (alpha color) 255) 1.0)))
    (declare (type fixnum x y width height))
    (declare (type real r g b a))
    (gl:color r g b a)
    (gl:disable :texture-2d)
    (gl:with-primitive :polygon
      (gl:vertex x y 1)
      (gl:vertex x (+ y height) 1)
      (gl:vertex (+ x width) (+ y height) 1)
      (gl:vertex (+ x width) y 1))
    (gl:enable :texture-2d)))

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

