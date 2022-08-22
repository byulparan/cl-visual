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
  (ns:objc syphon-server "publishFrameTexture:textureTarget:imageRegion:textureDimensions:flipped:"
	   :unsigned-int tex-id
	   :int texture-target
	   (:struct ns:rect) region
	   (:struct ns:size) size
	   :int 0))

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
  (ns:objc syphon-image "textureSize" (:struct ns:size)))

(defun stop-client (syphon-client)
  (ns:objc syphon-client "stop"))



