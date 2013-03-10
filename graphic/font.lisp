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

#|
surface cache
----------

create surface is slow & use memory.
To avoid this overhead, memoize surface.

|#
(defparameter *solid-surface-cache* (make-hash-table :test 'equal))
(defparameter *blend-surface-cache* (make-hash-table :test 'equal))

(defun generate-font-key (string color)
  (format nil "~A:~A~A~A" string (red color) (green color) (blue color)))

(defun get-text-blend-surface (string &key color
					(font lispbuilder-sdl:*default-font*))
  (let ((key (generate-font-key string color)))
    (when (eq nil (nth-value 0 (gethash key *blend-surface-cache*)))
      (setf (gethash key *blend-surface-cache*)
	    (sdl:render-string-blended string
				       :font font :color (color-to-sdl-color color)
				       :free nil :cache nil)))
    (gethash key *solid-surface-cache*)))

(defun get-text-solid-surface (string &key color
					(font lispbuilder-sdl:*default-font*))
  (let ((key (generate-font-key string color)))
    (when (eq nil (nth-value 0 (gethash key *solid-surface-cache*)))
      (setf (gethash key *solid-surface-cache*)
	    (sdl:render-string-solid string :font font :color (color-to-sdl-color color)
				     :free nil :cache nil)))
    (gethash key *solid-surface-cache*)))


(defclass text-cache ()
  ((texture :initarg :texture :accessor texture)
   (surface :initarg :surface :accessor surface)
   (text :initarg :text :accessor text)
   (width :initarg :width :accessor font-width)
   (height :initarg :height :accessor font-height)))

(defparameter *text-cache* (make-hash-table :test 'equal))

(defun get-text-solid (text &key color (font lispbuilder-sdl:*default-font*))
  (let* ((font-surf (get-text-solid-surface text :font font :color color))
	 (w (sdl:width font-surf))
  	 (h (sdl:height font-surf))
	 (temp-surface (sdl:create-surface w h :bpp 32 :pixel-alpha t )))
    (sdl:blit-surface font-surf temp-surface)
    (make-instance 'text-cache
		   :texture (vml-image:surface-to-texture temp-surface)
		   :text text
		   :width w
		   :height h)))

(defun get-text-blended (text &key color (font lispbuilder-sdl:*default-font*))
  (let* ((font-surf (sdl:render-string-blended text 
					       :font font
					       :color (color-to-sdl-color color) 
					       :free t
					       :cache t))
	 (w (sdl:width font-surf))
	 (h (sdl:height font-surf))
	 (text-cache (make-instance 'text-cache
	  			    :texture (vml-image:surface-to-texture font-surf)
				    :text text
				    :width w
				    :height h)))
    text-cache))

@export
(defun get-text (text &key color
		 (font lispbuilder-sdl:*default-font*)
		 (type :blend))
  (case type
    (:blend (get-text-blended text :color color :font font))
    (:solid (get-text-solid text :color color :font font))))
	    
@export 
(defun draw-font (text point color
		  &key (font lispbuilder-sdl:*default-font*) (type :blend))
  (let* ((x (x point))
	 (y (y point))
	 (text-texture (get-text text :color color :font font :type type))
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


;; #+sbcl
;; (progn
;;   (sb-profile:profile init-fonts)
;;   (sb-profile:profile get-text-solid)
;;   (sb-profile:profile get-text-blended)
;;   (sb-profile:profile get-text)
;;   (sb-profile:profile draw-font))

