
(defpackage :cl-visual
  (:use :cl :alexandria)
  #-sbcl (:import-from :safe-queue
		      #:make-mailbox
		      #:mailbox-send-message
		      #:mailbox-receive-message
		      #:mailbox-empty-p)
  #+sbcl (:import-from :sb-concurrency
		       #:make-mailbox
		       #:send-message
		       #:receive-message
		       #:mailbox-empty-p)
  (:export #:*num-ivolume*
	   #:*num-icontrol*
	   #:*visual-volume-function*
	   #:*visual-control-function*
	   #:*visual-canvas-init-functions*
	   #:*visual-canvas-release-functions*
	   #:ichannel0
	   #:ichannel1
	   #:ichannel2
	   #:ichannel3
	   #:ichannel4
	   #:ichannel5
	   #:ichannel6
	   #:ichannel7

	   #:iglobal-time
	   #:itime
	   #:ivolume0
	   #:ivolume1
	   #:ivolume2
	   #:ivolume3
	   #:ivolume4
	   #:ivolume5
	   #:icontrol0
	   #:icontrol1
	   #:icontrol2
	   #:icontrol3
	   #:icontrol4
	   #:icontrol5
	   #:icontrol6
	   #:icontrol7
	   #:icontrol8
	   #:icontrol9
	   #:iresolution
	   #:camera
	   #:lookat
	   #:projection-matrix
	   #:modelview-matrix
	   #:imouse
	   #:vfuv))



(defpackage :ftgl
  (:use #:cl)
  (:export #:create-pixmap-font
	   #:create-texture-font
	   #:create-extrude-font
	   #:set-font-face-size
	   #:render-font
	   #:destroy-font))



(defpackage :syphon
  (:use :cl)
  (:export #:make-server
	   #:publish-frame
	   #:stop-server
	   #:available-servers
	   #:get-server
	   #:make-client
	   #:new-frame-image
	   #:valid-p
	   #:texture-name
	   #:texture-size
	   #:stop-client))


(defpackage :glsl
  (:import-from #:ppcre #:regex-replace-all)
  (:use #:cl #:alexandria #:split-sequence)
  (:export ;;#:defpipeline
	   #:v-defmacro
	   #:discard
	   #:lisp
	   #:true
	   #:false
	   #:initialize
	   #:s~
	   #:multf
	   #:cond!
	   #:if!
	   #:gl-point-size
	   #:gl-vertex-id
	   #:gl-instance-id
	   #:gl-frag-coord
	   #:gl-point-coord
	   #:gl-frag-depth
	   #:gl-depth-range
	   #:gl-position))

(defpackage :gfx
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

