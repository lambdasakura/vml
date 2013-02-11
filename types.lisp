(in-package #:cl-user)
(defpackage :vml-types
  (:documentation "Types in VML System")
  (:use :cl
	:cl-annot
	:cl-annot.class
	:cl-annot.doc
	:cl-store
	:cl-fad
	)
  (:nicknames :vml-types))
(in-package :vml-types)
(cl-annot:enable-annot-syntax)

@export
(defstruct point x y)

@export
(defun point (&key x y)
    (make-point x y))

@export
(defstruct rectangle x y width height)

@export
(defun rectangle (&key x y width height)
  (make-rectangle x y width height))

@export-accessors
@export
(defclass color ()
    ((red :initform 0 :initarg :r :accessor red)
     (green :initform  0 :initarg :g :accessor green)
     (blue :initform 0 :initarg :b :accessor blue)
     (alpha :initform nil :initarg :a :accessor alpha)))

@export
(defun color (&key (r 0) (g 0) (b 0) (a nil))
  (make-instance 'color :r r :g g :b  b :a a))

@export
(defun color-to-sdl-color (color)
  (if color 
      (sdl:color :r (red color)
		 :g (green color)
		 :b (blue color)
		 :a (alpha color))
      (sdl:*black*)))
