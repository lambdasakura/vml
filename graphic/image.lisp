(in-package :cl-user)
(defpackage vml-image
  (:nicknames :vml-image)
  (:use :cl
	:cl-annot
	:cl-annot.class
	:cl-annot.doc))
(in-package :vml-image)
(cl-annot:enable-annot-syntax)

@export
(defun surface-to-texture (surface)
  (let ((texture (car (gl:gen-textures 1)))
        (w (sdl:width surface))
	(h (sdl:height surface)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-image-2d
     :texture-2d 0 :rgba w h 0 :rgba :unsigned-byte
     (sdl-base::with-pixel (pixels (sdl:fp surface))
       (sdl-base::pixel-data pixels)))
    texture))
