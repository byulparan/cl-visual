(defpackage :cl-visual
  (:use :cl :alexandria)
  #+ccl (:import-from :safe-queue
		      #:make-mailbox
		      #:mailbox-send-message
		      #:mailbox-receive-message
		      #:mailbox-empty-p)
  #+sbcl (:import-from :sb-concurrency
		       #:make-mailbox
		       #:send-message
		       #:receive-message
		       #:mailbox-empty-p)
  (:export #:*icontrol-index*
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
	   #:lookat))

