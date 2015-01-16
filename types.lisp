;;;;------------------------------------------------------------------------;;;;
;;;; types.lisp drawing primitive.
;;;;
;;;; Date: 2013.03.31
;;;; Author: lambda_sakura(lambda.sakura@gmail.com)
;;;;
;;;;------------------------------------------------------------------------;;;;

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

@export-accessors
@export
(defclass point ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)))

@export
(defun point (&key x y)
  (make-instance 'point :x x :y y))

@export-accessors
@export
(defclass rectangle ()
  ((x :initform 0 :initarg :x  :accessor x)
   (y :initform 0 :initarg :y  :accessor y)
   (width :initform 0 :initarg :w  :accessor w)
   (height :initform 0 :initarg :h  :accessor h)))

@export
(defun rectangle (&key x y w h)
  (make-instance 'rectangle :x x :y y :w w :h h))

@export
(defmacro rect (&key x y w h)
  `(rectangle :x ,x :y ,y :w ,w :h ,h))



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
