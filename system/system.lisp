(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :swank)
    (pushnew :my-game-debug *features*)))

(in-package #:lispbuilder-sdl)
(cl-annot:enable-annot-syntax)

@export
(defmethod _render-string-shaded_ ((string string) (fg-color color) (bg-color color) (font ttf-font) free cache)
  (let ((surf nil))
    (with-foreign-color-copy (fg-struct fg-color)
      (with-foreign-color-copy (bg-struct bg-color)
        (multiple-value-bind (fg bg)
            (if (cffi:foreign-symbol-pointer "TTF_glue_RenderUTF8_Shaded")
              (values fg-struct bg-struct)
              (values (+ (ash (b fg-color) 16)
                         (ash (g fg-color) 8)
                         (r fg-color))
                      (+ (ash (b bg-color) 16)
                         (ash (g bg-color) 8)
                         (r bg-color))))
          (setf surf (make-instance 'surface :fp (sdl-ttf-cffi::render-utf8-shaded (fp font) string fg bg))))))
    (when cache
      (setf (cached-surface font) surf))
    surf))

@export
(defmethod _render-string-solid_ ((string string) (font ttf-font) (color color) free cache)
  (let ((surf nil))
    (with-foreign-color-copy (col-struct color)
      (setf surf (make-instance 'surface :fp (sdl-ttf-cffi::render-utf8-solid (fp font) string
                                                                                (if (cffi:foreign-symbol-pointer "TTF_glue_RenderUTF8_Solid")
                                                                                  col-struct
                                                                                  (+ (ash (b color) 16)
                                                                                     (ash (g color) 8)
                                                                                     (r color)))))))
    (when cache
      (setf (cached-surface font) surf))
    surf))

@export
(defmethod _render-string-blended_ ((string string) (font ttf-font) (color color) free cache)
  (let ((surf nil))
    (with-foreign-color-copy (col-struct color)
      (setf surf (make-instance 'surface :fp (sdl-ttf-cffi::render-utf8-blended (fp font) string
                                                                                (if (cffi:foreign-symbol-pointer "TTF_glue_RenderUTF8_Blended")
                                                                                  col-struct
                                                                                  (+ (ash (b color) 0)
                                                                                     (ash (g color) 8)
                                                                                     (ash (r color) 16)))))))
    (when cache
      (setf (cached-surface font) surf))
    surf))

(in-package #:cl-user)
(defpackage :vml-system
  (:use :cl
	:cl-annot
	:cl-annot.class
	:cl-annot.doc
	:cl-store
	:cl-fad
	:vml-input
	:vml-keyboard
	:vml-joystick)
  (:nicknames :vml-system))
(in-package :vml-system)
(cl-annot:enable-annot-syntax)


@export
(defclass vml-system ()
  ((game-title :initarg :game-title :initform nil :accessor game-title)
   (game-quit :initarg :game-quit :initform nil :accessor game-quit-func)
   (game-main :initarg :game-main :initform nil :accessor game-main-func)
   (game-init :initarg :game-init :initform nil :accessor game-init-func)
   (tex-reload :initarg :tex-reload :initform nil :accessor tex-reload)
   ))

@doc "initialize OpenGL settings."
(defun initialize-OpenGL ()
  (gl:enable :texture-2d)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0 0 0 0)
  (gl:enable :depth-test)
  (gl:depth-func :lequal)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun toggle-fullscreen ()
  (cond ((eql *fullscreen* t)
	 (setf *fullscreen* nil)
	 (change-window-mode))
	((eql *fullscreen* nil)
	 (setf *fullscreen* t)
	 (change-fullscreen))))

(defmethod change-fullscreen ((self vml-system))
  (sdl:window 640 480 :fullscreen t
	      :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1))
	      :BPP 24 
	      :icon-caption (game-title self)
	      :title-caption (game-title self)
	      :opengl t)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:clear-color 0.0 0.0 0.0  1.00)
  (gl:viewport 0 0 640 480)
  (setf (sdl:frame-rate) 60)
  (gl:enable :texture-2d)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0 0 0 0)
  (gl:enable :depth-test)
  (gl:depth-func :lequal)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  
  (funcall (tex-reload self)))

(defmethod change-window-mode ((self vml-system))
  (sdl:window 640 480 :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1))
	      :BPP 24 :opengl t
	      :icon-caption (game-title self)
	      :title-caption (game-title self) )
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:clear-color 0.0 0.0 0.0  1.00)
  (gl:viewport 0 0 640 480)
  (setf (sdl:frame-rate) 60)
  (gl:enable :texture-2d)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0 0 0 0)
  (gl:enable :depth-test)
  (gl:depth-func :lequal)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (funcall (tex-reload self)))


(defun game-quit-key-press-p (key mod-key)
  (and (eql key :SDL-KEY-F4)  
       (or (sdl:modifier= '(:sdl-key-mod-ralt)
			  mod-key)
	   (sdl:modifier= '(:sdl-key-mod-lalt)
			  mod-key))))

(defun fullscreen-key-press-p (key mod-key)

  (and (eql key :SDL-KEY-RETURN)  
       (or (sdl:modifier= '(:sdl-key-mod-lalt)
			  mod-key)
	   (sdl:modifier= '(:sdl-key-mod-ralt)
			  mod-key))))

@doc "To load SDL Library.
in Windows,the DLL load from PATH."
(defun load-dynamic-libs ()
  (cffi:define-foreign-library sdl
    (t (:default "SDL")))
  (cffi:define-foreign-library sdl-image
    (t (:default "SDL_image")))
  (cffi:define-foreign-library sdl-gfx
    (t (:default "SDL_gfx")))
  (cffi:define-foreign-library sdl-mixer
    (t (:default "SDL_mixer")))
  (cffi:define-foreign-library sdl-ttf
    (t (:default "SDL_ttf")))
  (cffi:use-foreign-library sdl)
  (cffi:use-foreign-library sdl-image)
  (cffi:use-foreign-library sdl-gfx)
  (cffi:use-foreign-library sdl-ttf)
  (cffi:use-foreign-library sdl-mixer))

   
(defmethod system-initialize ((self vml-system))
  (sdl:show-cursor nil)
  (sdl:window 640 480
	      :bpp 24 :opengl t 
	      :icon-caption (game-title self)
	      :title-caption (game-title self)
	      :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
  (gl:viewport 0 0 640 480)
  (sdl:disable-key-repeat)
  (setf (sdl:frame-rate) 60))


(defmethod vml-init ((self vml-system))
  (system-initialize self)
  (initialize-OpenGL)
  (vml-music:open-music)
  (sdl-mixer:allocate-channels 16)
  (sdl:enable-unicode)
  (vml-keyboard:init-keyboard)
  (vml-joystick:init-joystick)
  )

(defmethod vml-system-game-main ((self vml-system))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  
  (gl:clear-color 0.0 0.0 0.0  1.00)
  
  (gl:with-pushed-matrix
    (gl:ortho 0 640 480 0 -1 1)
    (gl:enable :blend)
    (gl:color 1 1 1)
    (funcall (game-main-func self))
    (gl:color 1 1 1))
  (gl:flush)
  (sdl:update-display))

@doc "Vml's entry function.
1. initialize vml system and user datas.
2. exec game main-loop-function each frame.(default fps is 60fps)
3. at exit game. exec quit-function.
"
@export
(defmethod game-start ((self vml-system))
  (sdl:with-init (sdl:SDL-INIT-VIDEO 
		  SDL:SDL-INIT-AUDIO
		  SDL:SDL-INIT-JOYSTICK)
    (vml-init self)
    (funcall (game-init-func self))
    (sdl:with-events ()
      (:quit-event () (funcall (game-quit-func self)) t)
       (:MOUSE-BUTTON-DOWN-EVENT (:BUTTON BUTTON :STATE STATE :X X :Y Y)  
				 &BODY BODY)  
       (:MOUSE-BUTTON-UP-EVENT (:BUTTON BUTTON :STATE STATE :X X :Y Y)  
			 &BODY BODY) 
      (:KEY-DOWN-EVENT (:KEY KEY :MOD-KEY mod-key)  
		       (vml-keyboard:down-key key)
		       (when (fullscreen-key-press-p key mod-key)
			 (toggle-fullscreen))
		       (when (game-quit-key-press-p key mod-key)
			 (SDL:PUSH-QUIT-EVENT))
		       (WHEN (eql KEY :SDL-KEY-ESCAPE)  
			 (cond ((eql *game-stopped* t)
				(setf *game-stopped* nil))
			       ((eql *game-stopped* nil)
				(setf *game-stopped* t)))))
      (:KEY-UP-EVENT ( :KEY KEY) (up-key key))
      (:JOY-AXIS-MOTION-EVENT 
       (:WHICH WHICH :AXIS AXIS :VALUE VALUE )
       (vml-joystick:handle-cursor-event axis value which))
      (:JOY-BUTTON-DOWN-EVENT
       (:WHICH WHICH :BUTTON BUTTON :STATE STATE)  
       (vml-joystick:handle-button-press-event button which))
      (:JOY-BUTTON-UP-EVENT
       (:WHICH WHICH :BUTTON BUTTON :STATE STATE)  
       (vml-joystick:handle-button-release-event button which))
      (:IDLE () 
	     (vml-joystick:joystick-update)
	     #+my-game-debug
	     (let ((connection
	     	    (or swank::*emacs-connection* (swank::default-connection))))
	       (when (and connection (not (eql swank:*communication-style* :spawn)))
	     	 (swank::handle-requests connection t)))
	     (vml-system-game-main self)))))


(defun clean-up ()
  (when (sdl-mixer:music-playing-p)
    (sdl-mixer:Pause-Music)
    (sdl-mixer:Halt-Music))
  (when (sdl-mixer:sample-playing-p nil)
    (sdl-mixer:pause-sample t)
    (sdl-mixer:Halt-sample :channel t))
  ;; (when *mixer-opened*
  ;;   (sdl-mixer:Close-Audio t)
  ;;   (setf *mixer-opened* nil))
  (sdl-mixer:quit-mixer))
