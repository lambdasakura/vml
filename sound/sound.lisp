;;;;------------------------------------------------------------------------;;;;
;;;; sound.lisp drawing primitive.
;;;;
;;;; Date: 2013.03.31
;;;; Author: lambda_sakura(lambda.sakura@gmail.com)
;;;;
;;;;------------------------------------------------------------------------;;;;
(in-package :cl-user)
(defpackage :vml-sound
  (:use :cl
	:cl-user)
  (:export
   :load-sound
   :play-sound))
(in-package :vml-sound)
(cl-annot:enable-annot-syntax)

(defparameter *sounds* (make-hash-table :test 'equal))

@export
(defun load-sound (id filename)
  (setf (gethash id *sounds*) 
  	(sdl-mixer:load-sample
	 (kmrcl:read-file-to-usb8-array filename)))
  (setf (lispbuilder-sdl-mixer:sample-volume (gethash id *sounds*)) 50))

(defun sound (id)
  (gethash id *sounds*))

@export
(defun play-sound (id)
  (sdl-mixer:play-sample (sound id)))
