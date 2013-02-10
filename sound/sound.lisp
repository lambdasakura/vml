(in-package :vml-sound)
(defparameter *sounds* (make-hash-table :test 'equal))

(defun load-sound (id filename)
  (setf (gethash id *sounds*) 
  	(sdl-mixer:load-sample (kmrcl:read-file-to-usb8-array filename)))
  (setf (lispbuilder-sdl-mixer:sample-volume (gethash id *sounds*)) 50))

(defun sound (id)
  (gethash id *sounds*))

(defun play-sound (id)
  (sdl-mixer:play-sample (sound id)))
