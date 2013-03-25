(in-package :cl-user)

(defpackage vml-fonts
  (:nicknames :vml-fonts)
  (:use :cl
	:cl-annot
	:cl-annot.class
	:cl-annot.doc
	:vml-types))

(in-package :vml-fonts)
(cl-annot:enable-annot-syntax)

#|
-----------------------------------------------------
surface cache
-----------------------------------------------------

create surface is slow & use memory.
To avoid this overhead, memoize surface.
-----------------------------------------------------
|#
(defparameter *solid-surface-cache* (make-hash-table :test 'equal))
(defparameter *blend-surface-cache* (make-hash-table :test 'equal))

(defun generate-font-key (string color)
  "generate hash key for surface cache from Color num and Text"
  (format nil "~A:~A~A~A" string (red color) (green color) (blue color)))

(defun generate-text-surface (string func &key 
				  color surf-cache
				  (font lispbuilder-sdl:*default-font*))
  (let ((key (generate-font-key string color)))
    (when (eq nil (nth-value 0 (gethash key surf-cache)))
      (setf (gethash key surf-cache)  (funcall func 
				       string
				       :font font
				       :color (color-to-sdl-color color)
				       :free nil 
				       :cache nil)))
    (gethash key surf-cache)))


(defun generate-text-surface-blend (string &key color
					(font lispbuilder-sdl:*default-font*))
  "generate text surface for blend."
  (generate-text-surface string #'sdl:render-string-blended
		    :color color :surf-cache *blend-surface-cache* :font font))

(defun generate-text-surface-solid (string &key color
					(font lispbuilder-sdl:*default-font*))
  "generate text surface for solid"
  (generate-text-surface string #'sdl:render-string-solid
		    :color color :surf-cache *solid-surface-cache* :font font))


#|
-----------------------------------------------------
text cache
-----------------------------------------------------

In OpenGL,there is a limit to the number of create textures.
so we cached textures in hash table.

inprogress to implements :(
-----------------------------------------------------
|#

(defclass text-texture-cache ()
  ((texture :initarg :texture :accessor texture)
   (surface :initarg :surface :accessor surface)
   (text :initarg :text :accessor text)
   (width :initarg :width :accessor font-width)
   (height :initarg :height :accessor font-height)))

(defparameter *text-texture-cache* (make-hash-table :test 'equal))

(defun generate-text-texture-solid (text &key color 
				      (font lispbuilder-sdl:*default-font*))
  (let* ((font-surf (generate-text-surface-solid text :font font :color color))
	 (w (sdl:width font-surf))
  	 (h (sdl:height font-surf))
	 (temp-surface (sdl:create-surface w h :bpp 32 :pixel-alpha t )))
    (sdl:blit-surface font-surf temp-surface)
    (make-instance 'text-texture-cache
		   :texture (vml-image:surface-to-texture temp-surface)
		   :text text
		   :width w
		   :height h)))

(defun generate-text-texture-blend (text &key color
					(font lispbuilder-sdl:*default-font*))
  (let* ((font-surf (generate-text-surface-blend text :font font :color color))
	 (w (sdl:width font-surf))
	 (h (sdl:height font-surf))
	 (text-texture-cache (make-instance 'text-texture-cache
	  			    :texture (vml-image:surface-to-texture font-surf)
				    :text text
				    :width w
				    :height h)))
    text-texture-cache))

(defun generate-text-texture (text &key color
		 (font lispbuilder-sdl:*default-font*)
		 (type :blend))
  (case type
    (:blend (generate-text-texture-blend text :color color :font font))
    (:solid (generate-text-texture-solid text :color color :font font))))
	    
@export
(defun draw-font (text point color
		  &key (font lispbuilder-sdl:*default-font*) (type :blend))
  (let* ((x (x point))
	 (y (y point))
	 (text-texture (generate-text-texture text :color color :font font :type type))
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

@export
(defun init-fonts ()
  (unless (sdl:initialise-default-font sdl:*ttf-font-vera*)
    (error "FONT-EXAMPLE: Cannot initialize the default font.")))

@export
(defun load-font (filename size)
  (unless (sdl:initialise-default-font 
	   (make-instance 'sdl:ttf-font-definition
			  :size size
			  :filename  filename))
    (error "FONT-EXAMPLE: Cannot initialize the default font.")))

;; #+sbcl
;; (progn
;;   (sb-profile:profile init-fonts)
;;   (sb-profile:profile get-text-solid)
;;   (sb-profile:profile get-text-blended)
;;   (sb-profile:profile get-text)
;;   (sb-profile:profile draw-font))

