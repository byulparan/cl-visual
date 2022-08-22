(in-package :gfx)

(defclass %framebuffer-object ()
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (framebuffer :initarg :framebuffer :reader framebuffer)
   (colorbuffer :initarg :colorbuffer :reader colorbuffer)
   (depthbuffer :initarg :depthbuffer :reader depthbuffer)))

(defclass framebuffer-object (%framebuffer-object)
  ((texture-target :initarg :texture-target :reader texture-target)
   (ext-texture-p :initarg :ext-tuxture-p :reader ext-texture-p)))

(defclass multisample-framebuffer-object (%framebuffer-object)
  ((output-fbo :initarg :output-fbo :reader output-fbo)))

(defmethod reinit-fbo ((fbo framebuffer-object) width height)
  (let* ((framebuffer (framebuffer fbo))
	 (texture (colorbuffer fbo))
	 (target (texture-target fbo))
	 (depthbuffer (depthbuffer fbo)))
    (setf (width fbo) width (height fbo) height)
    (gl:bind-framebuffer :framebuffer framebuffer)
    (unless (ext-texture-p fbo) 
      (gl:bind-texture target texture)
      (gl:tex-image-2d  target 0 :rgba8 width height 0 :rgba :unsigned-byte (cffi:null-pointer))
      (gl:tex-parameter target :texture-min-filter :linear)
      (gl:tex-parameter target :texture-mag-filter :linear)
      (gl:tex-parameter target :texture-wrap-s :clamp-to-edge)
      (gl:tex-parameter target :texture-wrap-t :clamp-to-edge)
      (gl:bind-texture target 0))
    (gl:framebuffer-texture-2d :framebuffer :color-attachment0 target texture 0)
    (gl:bind-renderbuffer :renderbuffer depthbuffer)
    (gl:renderbuffer-storage :renderbuffer :depth-component width height)
    (gl:bind-renderbuffer :renderbuffer 0)
    (gl:framebuffer-renderbuffer :framebuffer :depth-attachment :renderbuffer depthbuffer)
    (unless (eql :framebuffer-complete-oes (gl:check-framebuffer-status-ext :framebuffer))
      (error "can't (re)initialize framebuffer"))
    (gl:bind-framebuffer :framebuffer 0)
    fbo))

(defmethod reinit-fbo ((fbo multisample-framebuffer-object) width height)
  (reinit-fbo (output-fbo fbo) width height)
  (setf (width fbo) width (height fbo) height)
  (let* ((framebuffer (framebuffer fbo))
	 (colorbuffer (colorbuffer fbo))
	 (depthbuffer (depthbuffer fbo)))
    (gl:bind-framebuffer :framebuffer framebuffer)
    (gl:bind-renderbuffer :renderbuffer colorbuffer)
    (gl:renderbuffer-storage-multisample :renderbuffer 4 :rgba8 width height)
    (gl:bind-renderbuffer :renderbuffer 0)
    (gl:framebuffer-renderbuffer :framebuffer :color-attachment0 :renderbuffer colorbuffer)
    (gl:bind-renderbuffer :renderbuffer depthbuffer)
    (gl:renderbuffer-storage-multisample :renderbuffer 4 :depth-component width height)
    (gl:bind-renderbuffer :renderbuffer 0)
    (gl:framebuffer-renderbuffer :framebuffer :depth-attachment :renderbuffer depthbuffer)
    (unless (eql :framebuffer-complete-oes (gl:check-framebuffer-status-ext :framebuffer))
      (error "can't (re)initialize multisample-framebuffer"))
    (gl:bind-framebuffer :framebuffer 0)
    fbo))

(defun make-fbo (width height &key multisample texture (target :texture-2d))
  (let* ((fbo (let* ((framebuffer (gl:gen-framebuffer))
		     (colorbuffer (if texture texture (gl:gen-texture)))
		     (depthbuffer (gl:gen-renderbuffer)))
		(make-instance 'framebuffer-object
		  :width width :height height :framebuffer framebuffer
		  :colorbuffer colorbuffer :texture-target target :depthbuffer depthbuffer
		  :ext-tuxture-p (if texture t nil)))))
    (if multisample (let* ((framebuffer (gl:gen-framebuffer))
			   (colorbuffer (gl:gen-renderbuffer))
			   (depthbuffer (gl:gen-renderbuffer)))
		      (reinit-fbo
		       (make-instance 'multisample-framebuffer-object
			 :width width :height height :framebuffer framebuffer
			 :colorbuffer colorbuffer :depthbuffer depthbuffer
			 :output-fbo fbo)
		       width height))
      (reinit-fbo fbo width height))))


(defun release-fbo (fbo)
  (gl:delete-renderbuffers (list (depthbuffer fbo)))
  (if (typep fbo 'multisample-framebuffer-object) (progn
						    (gl:delete-renderbuffers (list (colorbuffer fbo)))
						    (release-fbo (output-fbo fbo)))
    (unless (ext-texture-p fbo) (gl:delete-texture (colorbuffer fbo))))
  (gl:delete-framebuffers (list (framebuffer fbo))))

(defun output-texture (fbo)
  (if (typep fbo 'multisample-framebuffer-object) (output-texture (output-fbo fbo))
    (colorbuffer fbo)))

(defun resolve-fbo (multisample-framebuffer-object)
  (let* ((draw-fbo (output-fbo multisample-framebuffer-object))
	 (read-fbo multisample-framebuffer-object))
    (gl:bind-framebuffer :draw-framebuffer (framebuffer draw-fbo))
    (gl:bind-framebuffer :read-framebuffer (framebuffer read-fbo))
    (%gl:blit-framebuffer 0 0 (width read-fbo) (height read-fbo) 0 0
			  (width draw-fbo) (height draw-fbo)
			  (logior
			   (cffi:foreign-enum-value '%gl:enum :color-buffer-bit)
			   (cffi:foreign-enum-value '%gl:enum :depth-buffer-bit))
			  :nearest)))

(defvar *fbo-stack* nil)

(defmacro with-fbo ((fbo) &body body)
  (alexandria:with-gensyms (fbo-id)
    `(let* ((,fbo-id (framebuffer ,fbo)))
       (when *fbo-stack* (push ,fbo-id  *fbo-stack*))
       (unwind-protect (progn
			 (gl:bind-framebuffer :framebuffer ,fbo-id)
			 ,@body)
	 (when (typep ,fbo 'multisample-framebuffer-object)
	   (resolve-fbo ,fbo))
	 (when *fbo-stack* (pop *fbo-stack*))
	 (gl:bind-framebuffer :framebuffer (if *fbo-stack* (car *fbo-stack*) 0))))))


