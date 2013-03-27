;;;;------------------------------------------------------------------------;;;;
;;;; font.lisp manipulating font drawing 
;;;;
;;;; Date: 2013.03.25
;;;; Author: lambda_sakura(lambda.sakura@gmail.com)
;;;;
;;;;------------------------------------------------------------------------;;;;
;;;;
;;;; This file implements font render which is to draw string with TTF font.
;;;;
;;;;------------------------------------------------------------------------;;;;

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

;;;-----------------------------------------------------------------------------
;;; Generate SDL Surface
;;;-----------------------------------------------------------------------------
(defun generate-text-surface-blend (string &key color
					     (font lispbuilder-sdl:*default-font*))
  "generate text surface for blend."
  (let* ((text-surface (sdl:render-string-blended string
						  :color (color-to-sdl-color color)
						  :free nil 
						  :cache nil
						  :font font))
	 (width (sdl:width text-surface))
  	 (height (sdl:height text-surface))
	 (temp-surface (sdl:create-surface width
					   height 
					   :bpp 32
					   )))
    (sdl:blit-surface text-surface temp-surface)
    text-surface))

(defun generate-text-surface-solid (string &key color
					     (font lispbuilder-sdl:*default-font*))
  "generate text surface for solid"
  (let* ((text-surface (sdl:render-string-solid string
						:color (color-to-sdl-color color)
						:free nil 
						:cache nil
						:font font))
	 (width (sdl:width text-surface))
  	 (height (sdl:height text-surface))
	 (temp-surface (sdl:create-surface width
					   height 
					   :pixel-alpha t
					   :bpp 32)))
    (sdl:blit-surface text-surface temp-surface)
    (sdl:convert-to-display-format 
     :surface temp-surface)))

(defun generate-text-surface (text &key color
		 (font lispbuilder-sdl:*default-font*)
		 (type :blend))
  (case type
    (:blend (generate-text-surface-blend text :color color :font font))
    (:solid (generate-text-surface-solid text :color color :font font))))


;;;-----------------------------------------------------------------------------
;;;surface cache
;;;-----------------------------------------------------------------------------
;;; create surface is slow & use memory.
;;; To avoid this overhead, memoize surface.
;;;-----------------------------------------------------------------------------
(defparameter *text-texture-cache* (make-hash-table :test 'equal))

(defparameter *solid-surface-cache* (make-hash-table :test 'equal)
  "Cache for Solid String Surface")
(defparameter *blend-surface-cache* (make-hash-table :test 'equal)
  "Cache for Blend String Surface")

(defun select-surface-cache (type)
  (case type
    (:blend *blend-surface-cache*)
    (:solid *solid-surface-cache*)))


@doc  "generate hash key for surface cache from Color num and Text"
(defun generate-font-key (string color)
  (format nil "~A:~A~A~A" string (red color) (green color) (blue color)))

@doc "Add SDL Surface to Surface cache"
(defun add-text-surface-to-cache (key surf-cache surface)
  (setf (gethash key surf-cache) surface))

(defun not-exist-cache-p (key surf-cache)
  (eq nil (nth-value 0 (gethash key surf-cache))))

(defun get-text-surface-from-cache (text
				    color 
				    type
				    &optional (font 
					       lispbuilder-sdl:*default-font*))
  (let ((key (generate-font-key text color))
	(surf-cache (select-surface-cache type)))
    (when (not-exist-cache-p key surf-cache)
      (add-text-surface-to-cache  key
				  surf-cache
				  (generate-text-surface text
							 :type type
							 :color color
							 :font font)))
    (gethash key surf-cache)))

;;;-----------------------------------------------------------------------------
;;;text cache
;;;-----------------------------------------------------------------------------
;;; 
;;;In OpenGL,there is a limit to the number of create textures.
;;;so we cached textures in hash table.
;;; 
;;;inprogress to implements :(
;;;-----------------------------------------------------------------------------
(defclass text-texture-cache ()
  ((texture :initarg :texture :accessor texture)
   (surface :initarg :surface :accessor surface)
   (text :initarg :text :accessor text)
   (width :initarg :width :accessor font-width)
   (height :initarg :height :accessor font-height)))

(defun cache-text-texture (surf-cache texture text color)
  (let ((key (generate-font-key text color)))
    (when (eq nil (nth-value 0 (gethash key surf-cache)))
      (setf (gethash key surf-cache) texture))))


;;;-----------------------------------------------------------------------------
;;; Generate OpenGL Texture
;;;-----------------------------------------------------------------------------
(defun generate-text-texture-helper (text 
				     &key color type
				       (font lispbuilder-sdl:*default-font*))
  (let* ((font-surf (get-text-surface-from-cache text 
						 color
						 type
						 font))
	 (width (sdl:width font-surf))
	 (height (sdl:height font-surf)))
    (make-instance 'text-texture-cache
		   :texture (vml-image:generate-texture-from-surface 
			     font-surf)
		   :text text
		   :width width
		   :height height)))


(defun generate-text-texture (text &key color
		 (font lispbuilder-sdl:*default-font*)
		 (type :blend))
  (generate-text-texture-helper text 
				:type type
				:color color
				:font font))
	    
;;;-----------------------------------------------------------------------------
;;; Drawing Function
;;;-----------------------------------------------------------------------------

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

