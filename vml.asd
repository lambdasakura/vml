(defsystem "vml"
  :name "VML"
  :author "lambda_sakura <lambda.sakura@gmail.com>"
  :version "0.0.1"
  :components 
  ((:module crypt
  	   :components 
  	   ((:file "generate-container")))
   (:module input
  	   :components 
  	   ((:file "packages")
  	    (:file "keyboard" :depends-on ("packages"))
  	    (:file "joystick" :depends-on ("packages"))))

   (:module music
	     :components 
  	    ((:file "packages")
  	     (:file "music" :depends-on ("packages"))
  	    ))
   (:module sound
  	    :components 
  	    ((:file "packages")
  	     (:file "sound" :depends-on ("packages"))))
   (:module graphic
  	    :components 
  	    ((:file "primitive")))
   (:module system
  	    :components 
  	    ((:file "system"))))
  :serial t

  :depends-on
  (:lispbuilder-sdl
   :lispbuilder-sdl-gfx
   :lispbuilder-sdl-image
   :lispbuilder-sdl-ttf
   :lispbuilder-sdl-mixer
   :cl-ppcre
   :cl-annot
   :cl-test-more
   :cl-opengl
   :cl-store
   :cl-fad
   :kmrcl
   ))
