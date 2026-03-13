(in-package :syphon)

(cffi:define-foreign-library syphon
  (:darwin (:framework "Syphon")))

(cffi:use-foreign-library syphon)

(defun make-server (name cgl-context)
  (ns:objc (ns:alloc "SyphonServer")
	   "initWithName:context:options:" :pointer (ns:autorelease (ns:make-ns-string name))
	   :pointer cgl-context
	   :pointer (cffi:null-pointer)
	   :pointer))

(defun publish-frame (syphon-server tex-id texture-target region size)
  (ns::with-sb-alien-rect (region region)
    (sb-alien:with-alien ((%size (sb-alien:struct ns:size)))
      (setf (sb-alien:slot %size 'ns:width) (float (ns:size-width size) 1.0d0)
	    (sb-alien:slot %size 'ns:height) (float (ns:size-height size) 1.0d0))
      (sb-alien:alien-funcall
       (sb-alien:extern-alien "objc_msgSend" (sb-alien:function sb-alien:void
								sb-alien:system-area-pointer
								sb-alien:system-area-pointer
								sb-alien:unsigned-int
								sb-alien:int
								(sb-alien:struct ns:rect)
								(sb-alien:struct ns:size)
								sb-alien:int))
       (ns::cocoa-ref syphon-server)
       (ns:sel "publishFrameTexture:textureTarget:imageRegion:textureDimensions:flipped:")
       tex-id
       texture-target
       region
       %size
       0))))


(defun stop-server (syphon-server)
  (ns:objc syphon-server "stop"))

(defun get-server-app-name (syphon-server)
  (ns:ns-string-to-lisp
   (ns:objc syphon-server "objectForKey:"
	    :pointer (ns:autorelease (ns:make-ns-string "SyphonServerDescriptionAppNameKey"))
	    :pointer)))

(defun get-server-name (syphon-server)
  (ns:ns-string-to-lisp
   (ns:objc syphon-server "objectForKey:"
	    :pointer (ns:autorelease (ns:make-ns-string "SyphonServerDescriptionNameKey"))
	    :pointer)))

(defun available-servers ()
  (ns:with-event-loop (:waitp t)
    (let* ((servers (ns:objc (ns:objc "SyphonServerDirectory" "sharedDirectory" :pointer)
			     "servers" :pointer)))
      (dotimes (i (ns:objc servers "count" :unsigned-int))
	(let* ((server (ns:objc servers "objectAtIndex:" :int i :pointer)))
	  (format t "[~d] ~a:~a~%" i (get-server-app-name server) (get-server-name server)))))))

(defun get-server (app-name name)
  (unless app-name (setf app-name ""))
  (unless name (setf name ""))
  (let* ((servers (ns:objc (ns:objc "SyphonServerDirectory" "sharedDirectory" :pointer)
			   "servers" :pointer))
	 (result nil))
    (dotimes (i (ns:objc servers "count" :unsigned-int))
      (let* ((server (ns:objc servers "objectAtIndex:" :int i :pointer)))
	(when (and (string= app-name (get-server-app-name server))
		   (string= name (get-server-name server)))
	  (setf result server)
	  (return))))
    result))

(defun make-client (description cgl-context)
  (ns:objc (ns:alloc "SyphonClient")
	   "initWithServerDescription:context:options:newFrameHandler:"
	   :pointer description
	   :pointer cgl-context
	   :pointer (cffi:null-pointer)
	   :pointer (cffi:null-pointer)
	   :pointer))

(defun new-frame-image (syphon-client)
  (ns:objc syphon-client "newFrameImage" :pointer))

(defun valid-p (syphon-client)
  (ns:objc syphon-client "isValid" :bool))

(defun texture-name (syphon-image)
  (ns:objc syphon-image "textureName" :unsigned-int))

(defun texture-size (syphon-image)
  (sb-alien:with-alien ((%size (sb-alien:struct ns:size)))
    (sb-alien:alien-funcall-into
     (sb-alien:extern-alien "objc_msgSend" (sb-alien:function (sb-alien:struct ns:size)
							      sb-alien:system-area-pointer
							      sb-alien:system-area-pointer))
     (sb-alien:alien-sap %size)
     (ns::cocoa-ref syphon-image)
     (ns:sel "textureSize"))
    (ns:size (sb-alien:slot %size 'ns:width)
	     (sb-alien:slot %size 'ns:height))))

(defun stop-client (syphon-client)
  (ns:objc syphon-client "stop"))



