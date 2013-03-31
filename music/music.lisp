;;;;------------------------------------------------------------------------;;;;
;;;; music.lisp drawing primitive.
;;;;
;;;; Date: 2013.03.31
;;;; Author: lambda_sakura(lambda.sakura@gmail.com)
;;;;
;;;;------------------------------------------------------------------------;;;;
(in-package #:cl-user)
(defpackage #:vml-music
  (:use :cl
	:cl-user
	:kmrcl
	)
  (:export
   :load-music-data
   :play-music
   :pause-music
   :stop-music
   :open-music
   :close-music))
(in-package :vml-music)
(cl-annot:enable-annot-syntax)

@export
(defparameter *musics* (make-hash-table :test 'equal))
(defparameter *play-state* nil)

(defclass play-state ()
  ((playing-id :initform nil :accessor playing-id)
   (playing-music :initform nil :accessor playing-music)
   (playing-obj :initform nil :accessor playing-obj)
   (playing :initform nil :accessor playing)))

(defmethod load-data ((self play-state) id)
  (mv-bind (music-obj music-binary) (music id)
    (setf (playing-music self) music-obj)
    (setf (playing-obj self) music-binary)))

@export
(defun open-music ()
  (setf *musics* nil)
  (setf *play-state* (make-instance 'play-state))
  (setf *musics* (make-hash-table :test 'equal))
  (sdl-mixer:close-audio)
  (sdl-mixer:quit-mixer)

  (sdl-mixer:init-mixer :ogg)
  (setf *mixer-opened* 
	(sdl-mixer:OPEN-AUDIO :chunksize 2048
			      :enable-callbacks nil)))

@export
(defun close-music ()
  (sdl-mixer:halt-Music)
  (setf *musics* nil)
  (setf (playing-obj *play-state*) nil)
  (setf (playing-id *play-state*) nil))


@export
(defmethod load-music-data (id (data vector))
  (setf (gethash id *musics*) data))

(defmethod load-music-data (id (filename string))
  (setf (gethash id *musics*)
	(read-file-to-usb8-array filename)))

(defun music (id)
  ;; (sdl-mixer:load-music 
  ;;  (gethash id *musics*))
  )

@export
(defun play-music (id)
  (when (not (eq (playing-id *play-state*) id))
    (setf (playing *play-state*) nil)
    (sdl-mixer:pause-Music)
    (sdl-mixer:Halt-Music))
  
  (when (eq (playing *play-state*) nil)
    (setf (playing *play-state*) t)
    (setf (playing-id *play-state*) id)

    (load-data *play-state* id)
    
    ;; (when  (playing-music *play-state*)
    ;;   (sdl-mixer:play-music (playing-music *play-state*) :loop t))
    ))

@export
(defun pause-music()
  "音楽を一時停止する"
  (sdl-mixer:pause-Music))

@export
(defun stop-music()
  "音楽を停止する。"
  (sdl-mixer:halt-Music  1000)
  (sdl-mixer:halt-Music)
  (sdl-mixer:halt-Music)
  (when *play-state*
    (setf (playing-id *play-state*) nil)))



