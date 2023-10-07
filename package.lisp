
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
	   #:view-matrix
	   #:imouse
	   #:vfuv

	   #:*visual-canvas*))

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

(defpackage :post-fx
  (:use :cl :glsl)
  (:export #:bloom
	   #:set-bloom))

