;;;;------------------------------------------------------------------------;;;;
;;;; image.lisp manipulating image(bmp,png) 
;;;;
;;;; Date: 2013.03.25
;;;; Author: lambda_sakura(lambda.sakura@gmail.com)
;;;;
;;;;------------------------------------------------------------------------;;;;
;;;;
;;;; This file implements image loader which is to load image from file 
;;;; or stream(unsigned byte array).Loaded image convert to OpenGL texture and
;;;; SDL Surface.VML maintain loaded image *texture* and texture's id.
;;;;
;;;; This file also implements image drawer which is to draw loaded image.
;;;;
;;;;------------------------------------------------------------------------;;;;
(in-package :cl-user)
(defpackage vml-image
  (:nicknames :vml-image)
  (:use :cl
	:cl-annot
	:cl-annot.class
	:cl-annot.doc))
(in-package :vml-image)
(cl-annot:enable-annot-syntax)

;;;-----------------------------------------------------------------------------
;;; Loaded Image and Image cache for SDL_Surface and GL's Texture.
;;;-----------------------------------------------------------------------------
(defparameter *sdl-surface-cache* (make-array 0 :fill-pointer 0 :adjustable t)
  "SDL surface cache")

;;;=== Notice ==================================================================
;;;
;;; When change screen setting(fullscreen -> window,window -> fullscreen),
;;; We +must+ to regenerate all textures because GL Texture losted.
;;;
;;;=============================================================================
(defparameter *gl-texture-cache* (make-array 0 :fill-pointer 0 :adjustable t)
  "GL Texture cache")

;;;
;;; *texture* have all texture data which include surface 
;;; cache and gl's texture cache
;;;
(defparameter *texture* nil "All texture data")

;;;-----------------------------------------------------------------------------
;;; image converter functions.
;;;-----------------------------------------------------------------------------
@doc "Generate GL's Texture from SDL_Surface."
@export
(defun generate-texture-from-surface (surface)
  (check-type surface sdl:surface)
  (let ((texture (car (gl:gen-textures 1)))
        (w (sdl:width surface))
	(h (sdl:height surface)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-image-2d
     :texture-2d 0 :rgba w h 0 :rgba :unsigned-byte
     (sdl-base::with-pixel (pixels (sdl:fp surface))
       (sdl-base::pixel-data pixels)))
    texture))


@doc "Generate SDL_Surface from File."
(defun generate-surface-from-file (filename) 
  (let* ((image (sdl-image:load-image 
		 (kmrcl:read-file-to-usb8-array filename)))
	 (surf (sdl:create-surface (sdl:width image)
				   (sdl:height image)
				   :bpp 32
				   :pixel-alpha t)))
    (sdl:draw-surface-at-* image
			   0
			   0
			   :surface surf)
    surf))

@doc "Generate SDL_Surface from binary array."
(defun generate-surface-from-usb8-array (image-array) 
  (let* ((image (sdl-image:load-image image-array))
	 (surf (sdl:create-surface (sdl:width image)
				   (sdl:height image)
				   :bpp 32
				   :pixel-alpha t)))
    (sdl:draw-surface-at-* image
			   0
			   0
			   :surface surf)
    surf))

;;;-----------------------------------------------------------------------------
;;; maintain loaded images function
;;;-----------------------------------------------------------------------------
(defclass texture ()
  ((tex-id :initarg :id :accessor tex-id)
   (texture :initarg :texture :accessor texture)
   (surface :initarg :surface :accessor surface)
   (widht :initarg :width :accessor tex-width)
   (height :initarg :height :accessor tex-height)))

(defun add-new-texture (texture)
  (check-type texture (and texture (not null)))
  (vector-push-extend texture *texture*))

(defun get-texture (id)
  (check-type id (and integer (not null)))
  (aref *texture* id))



@export 
(defun initialize-textures ()
  (setf *texture* (make-array 0
			      :element-type 'texture
			      :fill-pointer 0
			      :adjustable t)))

;;;-----------------------------------------------------------------------------
;;; Image Loader 
;;;-----------------------------------------------------------------------------
@export
(defun load-image-file (filename)
  (check-type filename (and string (not null)))
  (let ((surface (generate-surface-from-file filename)))
    (add-new-texture (make-instance 'texture
				    :texture (generate-texture-from-surface surface)
				    :surface surface
				    :width (sdl:width surface)
				    :height  (sdl:height surface)))))
;;@export
;; (defun load-image-file (filename base-dir-len)
;;   (let ((surface (create-surface-with-alpha
;; 		  filename))
;; 	(key (subseq (format nil "~A" filename) base-dir-len)))
;;     (setf (gethash key *images*) 
;; 	  (make-instance 'texture
;; 			 ;;:texture nil
;; 			 :texture (generate-texture-from-surface surface)
;; 			 :surface surface
;; 			 :width (sdl:width surface)
;; 			 :height  (sdl:height surface)))
;;     (sdl:free surface)
;;     (when (equal (gethash key *images*) nil)
;;       (format t "error"))))



@export
(defun load-image (id filename path)
  (check-type id (and integer (not null)))
  (check-type filename (and string (not null)))
  (let ((surface (create-surface-with-alpha
		  (sdl:create-path filename path))))
    (setf (gethash id *images*) 
	  (make-instance 'texture
			 ;;:texture nil
			 :texture (generate-texture-from-surface surface)
			 :surface surface
			 :width (sdl:width surface)
			 :height  (sdl:height surface)))
    (sdl:free surface)
    (when (equal (gethash id *images*) nil)
      (format t "error"))))

@export
(defun draw-image (id x y &key (draw-reverse nil)
			(width nil) 
			(height nil)
			(r 1) (g 1) (b 1) (a 1))
  (let* ((surf (get-texture id))
	 (w (if width width (tex-width surf)))
	 (h (if height height (tex-height surf))))
    (gl:enable :texture-2d)
    (gl:bind-texture :texture-2d (texture surf))
    (gl:color r g b a)
    (cond
      ((eq t draw-reverse)
       (gl:with-primitive :quads
	 (gl:tex-coord 1 0) (gl:vertex x y 1)
	 (gl:tex-coord 0 0) (gl:vertex (+ x w) y 1)
	 (gl:tex-coord 0 1) (gl:vertex (+ x w) (+ y h) 1)
	 (gl:tex-coord 1 1) (gl:vertex x (+ y h) 1)))
      (t
       (gl:with-primitive :quads
	 (gl:tex-coord 0 0) (gl:vertex x y 1)
	 (gl:tex-coord 1 0) (gl:vertex (+ x w) y 1)
	 (gl:tex-coord 1 1) (gl:vertex (+ x w) (+ y h) 1)
	 (gl:tex-coord 0 1) (gl:vertex x (+ y h) 1))))
    (gl:disable :texture-2d)
    (gl:flush)))

