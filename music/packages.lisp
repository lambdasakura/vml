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
   :close-music
   ))
