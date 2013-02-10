(in-package #:cl-user)
(defpackage #:vml-joystick
  (:nicknames :joystick)
  (:use #:cl #:cl-user :kmrcl))

(defpackage #:vml-keyboard
  (:use #:cl #:cl-user :kmrcl)
  (:export
   :up-key
   :down-key
   :init-keyboard
   :keyboard-key1-state
   :keyboard-key2-state
   :keyboard-key3-state
   :keyboard-key4-state
   :keyboard-key5-state
   :keyboard-key6-state
   :keyboard-key7-state
   :keyboard-key8-state
   :keyboard-key9-state
   :keyboard-key0-state

   :keyboard-up-state 
   :keyboard-down-state 
   :keyboard-left-state 
   :keyboard-right-state 
   :keyboard-button1-state 
   :keyboard-button2-state
   :keyboard-button3-state
   ))


(defpackage #:vml-input
  (:use #:cl 
	#:cl-user
	#:vml-keyboard
	#:vml-joystick
	:kmrcl
	)
  (:export
   :exec-input-update
   
   :key1-pushed
   :key2-pushed
   :key3-pushed
   :key4-pushed
   :key5-pushed
   :key6-pushed
   :key7-pushed
   :key8-pushed
   :key9-pushed
   :key0-pushed

   :key1-pressed
   :key2-pressed
   :key3-pressed
   :key4-pressed
   :key5-pressed
   :key6-pressed
   :key7-pressed
   :key8-pressed
   :key9-pressed
   :key0-pressed

   :up-pressed
   :up-pushed
   :down-pressed
   :down-pushed
   :left-pressed
   :left-pushed
   :right-pressed
   :right-pushed
   :jump-pressed
   :jump-pushed
   :attack-pressed
   :attack-pushed
   :drop-item-pressed
   :drop-item-pushed
   ))
