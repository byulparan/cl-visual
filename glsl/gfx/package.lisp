(defpackage #:gfx
  (:use #:cl #:glsl)
  (:export #:deg-to-rad
	   #:get-internal-seconds
	   #:perspective-matrix
	   #:ortho-matrix

	   #:shader-environment
	   #:release-environment
	   
	   #:make-gpu-stream
	   #:gpu-stream-set
	   #:gpu-stream-length
	   #:gpu-stream-do-each
	   
	   #:make-shader-object
	   #:with-shader
	   
	   #:pull-g
	   #:defun-g
	   #:defvar-g
	   #:defstruct-g
	   #:defmacro-g

	   #:reinit-shader-system
	   #:defpipeline

	   #:make-fbo
	   #:reinit-fbo
	   #:width
	   #:height
	   #:output-texture 
	   #:release-fbo
	   #:with-fbo
	   #:*fbo-stack*
	   
	   #:camera
	   #:track-mouse-spin
	   #:track-mouse-zoom
	   #:track-mouse-pan
	   #:reset-camera
	   #:eval-camera
	   #:read-obj-file

	   #:load-quad-stream))

