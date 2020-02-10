(in-package :cl-visual)

(defvar *num-ivolume* 6)
(defvar *ivolume-index* 16300)
(defvar *num-icontrol* 10)
(defvar *icontrol-index* 16200)

(defclass renderer (gfx:gl-context)
  ((cgl-context
    :reader cgl-context)
   (core-profile
    :initarg :core-profile
    :initform t
    :reader core-profile)
   (pixel-format
    :reader pixel-format)
   (iosurface
    :initform nil
    :accessor iosurface)
   (texture
    :initform nil
    :accessor texture)
   (fbo 
    :initform nil
    :accessor fbo)
   (width
    :initarg :width
    :accessor width)
   (height
    :initarg :height
    :accessor height)))

(defmacro with-cgl-context ((cgl-context) &body body)
  `(let* ((current (cgl:get-current-context)))
     (unwind-protect (progn
		       (cgl:set-current-context ,cgl-context)
		       ,@body)
       (cgl:set-current-context current))))

(defmethod resize-framebuffer ((renderer renderer) width height)
  (setf (width renderer) width
	(height renderer) height)
  (let* ((cgl-context (cgl-context renderer)))
    (with-cgl-context (cgl-context)
      (when (iosurface renderer) (ns:release (iosurface renderer)))
      (setf (iosurface renderer) (io-surface:make-surface width height))
      (unless (texture renderer) (setf (texture renderer) (gl:gen-texture)))
      (gl:bind-texture :texture-rectangle (texture renderer))
      (cgl:tex-image-io-surface-2d cgl-context :texture-rectangle :rgba width height :bgra :unsigned-int-8-8-8-8-rev
				   (iosurface renderer) 0)
      (gl:bind-texture :texture-rectangle 0)
      (if (not (fbo renderer)) (setf (fbo renderer) (gfx:make-fbo width height
								  :multisample t
								  :texture (texture renderer)
								  :target :texture-rectangle))
	(gfx:reinit-fbo (fbo renderer) width height)))))


(defmethod initialize-instance :after ((instance renderer) &key)
  (let* ((%pixel-format (cgl:make-pixel-format (cgl:make-attributes :core-profile (core-profile instance))))
  	 (%cgl-context (cgl:make-context %pixel-format)))
    (with-slots (cgl-context pixel-format) instance
      (setf cgl-context %cgl-context
  	    pixel-format %pixel-format))))

(defmethod release ((renderer renderer))
  (with-cgl-context ((cgl-context renderer))
    (gfx:release-context renderer)
    (gfx:release-fbo (fbo renderer))
    (ns:release (iosurface renderer))
    (gl:delete-texture (texture renderer))
    (cgl:destroy-context (cgl-context renderer))
    (cgl:destroy-pixel-format (pixel-format renderer))))

;;; ================================================================================
;;;  visual-renderer
;;;
(defclass visual-renderer (renderer)
  ((gpu-stream
    :reader gpu-stream
    :initform (gfx:make-gpu-stream '((pos :vec2))
				   (list -1.0 -1.0 1.0 -1.0 -1.0 1.0 -1.0 1.0 1.0 -1.0 1.0 1.0)
				   :core-profile t))
   (shader
    :initform nil
    :accessor shader)
   (camera
    :initform (make-instance 'gfx:camera)
    :reader camera)
   (projection-matrix
    :accessor projection-matrix)
   (modelview-matrix
    :accessor modelview-matrix)
   (reinit-time
    :initarg :reinit-time
    :accessor reinit-time)
   (texture-devices
    :initform nil
    :accessor texture-devices)
   (tex-image-table
    :initform (make-hash-table :test #'equal)
    :reader tex-image-table
    :allocation :class)
   (gl-canvas
    :initform nil
    :accessor gl-canvas)))


;;; ===========================================================================
;;;
;;; for texture source
;;;

(defgeneric init-texture-device (view device texture-device)
  (:method (view device texture-device)
    (error "You should implementation this parse method ~a ~a" device texture-device)))

(defgeneric update-texture-device (view device texture-device))

(defgeneric release-texture-device (view device texture-device))


;;; ===========================================================================

(defun reinit-shader (renderer new-shader)
  (loop for (name shader-spec) on (gfx::shaders renderer) by #'cddr
	do (unless (eql name new-shader)
	     (destructuring-bind (build-time prog (vs fs))
		 shader-spec
	       (declare (ignore build-time))
	       (gl:detach-shader prog vs)
	       (gl:delete-shader vs)
	       (gl:detach-shader prog fs)
	       (gl:delete-shader fs)
	       (gl:delete-program prog))
	     (setf (gfx::shaders renderer) nil)))
  (setf (shader renderer) new-shader))

(defun reinit-textures (renderer options)
  (loop for device in (texture-devices renderer)
	do (release-texture-device renderer (car device) (cdr device)))
  (let* ((devices (getf options :textures)))
    (let* ((pipeline (gethash (shader renderer) gfx::*all-pipeline-table*))
	   (targets (mapcar (lambda (type) (ecase type 
					     (:sampler-2d :texture-2d)
					     (:sampler-2d-rect :texture-rectangle)))
			    (mapcar #'second (subseq (gfx::%pipeline-uniforms pipeline) 0 (length devices))))))
      (setf (texture-devices renderer)
	(loop for device in devices
	      for target in targets
	      collect (let ((device (alexandria:ensure-list device)))
			(init-texture-device renderer (car device) (append (cdr device)
									   (list :target target)))))))))

(defun reinit-visual-renderer (renderer options &optional scene-size)
  (with-cgl-context ((cgl-context renderer))
    (when scene-size
      (resize-framebuffer renderer (car scene-size) (second scene-size)))
    (reinit-shader renderer (getf options :shader))
    (reinit-textures renderer options)
    (when-let ((canvas (gl-canvas renderer)))
      (setf (gfx:width canvas) (width renderer)
	    (gfx:height canvas) (height renderer))
      (gfx:release canvas)
      (gfx:release-context canvas))
    (setf (gl-canvas renderer) nil)
    (when-let ((canvas (getf options :gl-canvas)))
      (setf (gl-canvas renderer) (make-instance canvas :camera (camera renderer)
						:width (width renderer) :height (height renderer)))
      (gfx:init (gl-canvas renderer)))))

(defun draw-shader (renderer w h)
  (setf (projection-matrix renderer) (kit.math:perspective-matrix 45.0 (/ w h) .1 10000.0)
	(modelview-matrix renderer) (gfx:eval-camera (camera renderer)))
  (let* ((time (funcall (reinit-time renderer))))
    (apply (shader renderer) renderer `(:triangles 0 6 ,(gpu-stream renderer)
					:ichannel0 0 :ichannel1 1 :ichannel2 2 :ichannel3 3
					:ichannel4 4 :ichannel5 5 :ichannel6 6 :ichannel7 7
					:iglobal-time ,time :itime ,time
					:ivolume0 ,(sc:control-get-sync *ivolume-index*)
					,@(loop for i from 0 below (- *num-ivolume* 1)
						append `(,(intern (format nil "IVOLUME~d" (+ i 1)) :keyword)
							 ,(sc:control-get-sync (+ *ivolume-index* i 1))))
					,@(loop for i from 0 below *num-icontrol*
						append
						`(,(intern (format nil "ICONTROL~d" i) :keyword)
						  ,(sc:control-get-sync (+ *icontrol-index* i))))
					:iresolution ,(list w h)
					:camera ,(list (gfx::eye-x (camera renderer))
						       (gfx::eye-y (camera renderer))
						       (gfx::eye-z (camera renderer)))
					:lookat ,(list (gfx::center-x (camera renderer))
						       (gfx::center-y (camera renderer))
						       (gfx::center-z (camera renderer)))
					:projection-matrix ,(projection-matrix renderer)
					:modelview-matrix ,(modelview-matrix renderer)))
    (when-let ((canvas (gl-canvas renderer)))
      (setf (gfx:width canvas) w (gfx:height canvas) h)
      (setf (gfx:projection-matrix canvas) (projection-matrix renderer)
	    (gfx:modelview-matrix canvas) (modelview-matrix renderer))
      (gfx:draw canvas))))

(defun render (renderer)
  (with-cgl-context ((cgl-context renderer))
    (let* ((w (width renderer))
	   (h (height renderer))
	   (draw-fbo (if (gl-canvas renderer) (fbo renderer)
		       (gfx::output-fbo (fbo renderer)))))
      (gfx:with-fbo (draw-fbo)
	(gl:viewport 0 0 w h)
	(gl:clear :color-buffer-bit :depth-buffer-bit)
	(loop for unit in '(:texture0 :texture1 :texture2 :texture3
			    :texture4 :texture5 :texture6 :texture7)
	      for device in (texture-devices renderer)
	      do (gl:active-texture unit)
		 (update-texture-device renderer (car device) (cdr device)))
	(gl:enable :depth-test)
	(draw-shader renderer w h)
	(gl:disable :depth-test))
      (gfx:with-fbo ((gfx::output-fbo (fbo renderer)))
	(loop for unit in '(:texture0 :texture1 :texture2 :texture3
			    :texture4 :texture5 :texture6 :texture7)
	      for device in (texture-devices renderer)
	      for target = (getf (cdr device) :target)
	      do (gl:active-texture unit)
		 (case (car device)
		   (:previous-frame
		    (gl:copy-tex-image-2d target 0 :rgba8 0 0 w h 0)))
		 (gl:bind-texture target 0)))) 
    (gl:flush)))

(defmethod release ((renderer visual-renderer))
  (with-cgl-context ((cgl-context renderer))
    (loop for device in (texture-devices renderer)
	  do (release-texture-device renderer (car device) (cdr device)))
    (call-next-method)))

