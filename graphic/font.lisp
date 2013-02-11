(in-package :cl-user)
(defpackage vml-fonts
  (:nicknames :vml-fonts)
  (:use :cl
	:cl-annot
	:cl-annot.class
	:cl-annot.doc
	:vml-types
	))
(in-package :vml-fonts)

(cl-annot:enable-annot-syntax)

@export
(defun init-fonts ()
  (unless (sdl:initialise-default-font sdl:*ttf-font-vera*)
    (error "FONT-EXAMPLE: Cannot initialize the default font.")))

(defparameter *text-cache* (make-hash-table :test 'equal))

(defclass text-cache ()
  ((texture :initarg :texture :accessor texture)
   (surface :initarg :surface :accessor surface)
   (text :initarg :text :accessor text)
   (width :initarg :width :accessor font-width)
   (height :initarg :height :accessor font-height)))

(defun get-text (text &key (color sdl:*black*) 
			(font lispbuilder-sdl:*default-font*))
  ;; sdl:render-string-blended
  (let* ((font-surf (sdl:render-string-blended text 
					     :font font
					     :color color 
					     :free t
					     :cache t))
  	 (w (sdl:width font-surf))
  	 (h (sdl:height font-surf))
	 (text-cache (make-instance 'text-cache
				    :texture (vml-image:surface-to-texture font-surf)
				    :text text
				    :width w
				    :height h)))
    text-cache
    ))


@export 
(defun draw-font (text point color &key (font lispbuilder-sdl:*default-font*))
  (let* ((x (x point))
	 (y (y point))
	 (text-texture (get-text text :color (color-to-sdl-color color) :font font))
	 (w (font-width text-texture))
  	 (h (font-height text-texture)))
    (gl:enable :texture-2d)
    (gl:bind-texture :texture-2d (texture text-texture))
    (gl:color 1.0 1.0 1.0 1.0)
    (gl:with-primitive :quads
      (gl:tex-coord 0 0) (gl:vertex x y 1)
      (gl:tex-coord 1 0) (gl:vertex (+ x w) y 1)
      (gl:tex-coord 1 1) (gl:vertex (+ x w) (+ y h) 1)
      (gl:tex-coord 0 1) (gl:vertex x (+ y  h) 1))
    (gl:delete-textures (list (texture text-texture)))
    (gl:disable :texture-2d)
    (gl:flush)))
