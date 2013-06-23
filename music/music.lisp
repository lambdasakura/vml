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
	:kmrcl)
  (:export 
    :open-music
    :close-music
    :load-music-data
    :play-music
    :pause-music
    :stop-music))

(in-package :vml-music)
(cl-annot:enable-annot-syntax)

@export
(defparameter *mixer-opened* nil)
(defparameter *play-state* nil)

;;; 
;;; SDLとのアダプタークラス。
;;;
;;; 音楽デバイスの状態を管理し、SDLへ適宜命令を発行する
;;;
(defclass play-state ()
  ((playing-id :initform nil :accessor playing-id)
   (playing-music :initform nil :accessor playing-music)
   (playing-obj :initform nil :accessor playing-obj)
   (playing :initform nil :accessor playing)))

;;;;------------------------------------------------------------------------;;;;
;;;;  初期化関連関数
;;;;------------------------------------------------------------------------;;;;
(defun open-music ()
  "音楽デバイスの初期化を行う。VML初期化時に一度実行する"
  ;; 音楽デバイスが開かれているのなら一旦閉じる
  (setf *musics* nil)
  (setf *play-state* (make-instance 'play-state))
  (setf *musics* (make-hash-table :test 'equal))
  (sdl-mixer:close-audio)
  (sdl-mixer:quit-mixer)

  ;; ogg vorvisを利用するモードで初期化する
  (sdl-mixer:init-mixer :ogg)
  (setf *mixer-opened* (sdl-mixer:OPEN-AUDIO :chunksize 2048 :enable-callbacks nil)))

(defun close-music ()
  "音楽デバイスの開放を行う。VML開放時に一度実行する"
  (sdl-mixer:halt-Music)
  (sdl-mixer:close-audio)
  (sdl-mixer:quit-mixer)

  (setf *musics* nil)
  (setf (playing-obj *play-state*) nil)
  (setf (playing-id *play-state*) nil))

;;;;------------------------------------------------------------------------;;;;
;;;;  音楽データ読み込み関数
;;;;------------------------------------------------------------------------;;;;
(let ((musics))

(defun init-music-data()
  "音楽データを初期化する"
  (setf musics (make-hash-table :test 'equal)))

(defmethod load-music-data (id (data vector))
  "引数で与えられた音楽データを指定されたidとして読み込む"
  (setf (gethash id musics) data))

(defmethod load-music-data (id (filename string))
  "引数で与えられたファイルパスの音楽データを指定されたidとして読み込む"
  (setf (gethash id musics)
	(read-file-to-usb8-array filename)))

(defun get-music-data (id)
 "指定されたidの音楽データを取得する"
 (gethash id musics)))

;;;;------------------------------------------------------------------------;;;;
;;;;  音楽再生の制御
;;;;------------------------------------------------------------------------;;;;
(defmethod load-data-to-sdl-mixer ((self play-state) id)
  "指定されたidの音楽データを次に再生される音楽データとして読み込む"
  (mv-bind (music-obj music-binary) (sdl-mixer:load-music (music id))
    (setf (playing-music self) music-obj)
    (setf (playing-obj self) music-binary))))

(defmethod stop-music-to-sdl-mixer ((self play-state))
  (sdl-mixer:pause-Music)
  (sdl-mixer:halt-Music))

(defun play-music (id)
  (when (not (eq (playing-id *play-state*) id))
    (setf (playing *play-state*) nil)
    (stop-music-to-sdl-mixer *player-state*))
  
  (when (eq (playing *play-state*) nil)
    (setf (playing *play-state*) t)
    (setf (playing-id *play-state*) id)

    (load-data-to-sdl-mixer *play-state* id)
    
    ;; (when  (playing-music *play-state*)
    ;;   (sdl-mixer:play-music (playing-music *play-state*) :loop t))
    ))

(defun pause-music()
  "音楽を一時停止する"
  (sdl-mixer:pause-Music))

(defun stop-music()
  "音楽を停止する。"
  (sdl-mixer:halt-Music)
  (when *play-state*
    (setf (playing-id *play-state*) nil)))



