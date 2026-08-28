(in-package :cl-visual)

(defclass renderer ()
  ((cgl-context
    :reader cgl-context)
   (pixel-format
    :reader pixel-format)
   (core-profile
    :initarg :core-profile
    :initform t
    :reader core-profile)
   (gl-format
    :initarg :format
    :initform :rgba8
    :reader gl-format)
   (iosurface
    :initform nil
    :accessor iosurface)
   (texture
    :initform nil
    :accessor texture)
   (fbo 
    :initform nil
    :accessor fbo)
   (multisample
    :initarg :multisample
    :initform nil
    :reader multisample)
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
  (let* ((cgl-context (cgl-context renderer))
	 (gl-format (gl-format renderer)))
    (with-cgl-context (cgl-context)
      (when (iosurface renderer) (ns:release (iosurface renderer)))
      (setf (iosurface renderer) (io-surface:make-surface width height :pixel-format (if (eql gl-format :rgba8) "ARGB" "RGfA")))
      (unless (texture renderer) (setf (texture renderer) (gl:gen-texture)))
      (gl:bind-texture :texture-rectangle (texture renderer))
      (if (eql gl-format :rgba8)
	  (cgl:tex-image-io-surface-2d cgl-context :texture-rectangle :rgba width height :bgra :unsigned-int-8-8-8-8-rev
				       (iosurface renderer) 0)
	(cgl:tex-image-io-surface-2d cgl-context :texture-rectangle :rgba32f width height :rgba :float
				     (iosurface renderer) 0))
      (gl:bind-texture :texture-rectangle 0)
      (if (not (fbo renderer)) (setf (fbo renderer) (gfx:make-fbo width height
								  :multisample (multisample renderer)
								  :texture (texture renderer)
								  :target :texture-rectangle
								  :format gl-format))
	(gfx:reinit-fbo (fbo renderer) width height)))))


(defmethod initialize-instance :after ((instance renderer) &key)
  (assert (find (gl-format instance) '(:rgba8 :rgba32f)) nil "Renderer Support only :rgba8 and :rgba32f")
  (let* ((%pixel-format (cgl:make-pixel-format (cgl:list-attributes :core-profile (core-profile instance))))
  	 (%cgl-context (cgl:make-context %pixel-format)))
    (with-slots (cgl-context pixel-format) instance
      (setf cgl-context %cgl-context
  	    pixel-format %pixel-format))))

(defmethod release ((renderer renderer))
  (with-cgl-context ((cgl-context renderer))
    (gfx:release-fbo (fbo renderer))
    (ns:release (iosurface renderer))
    (gl:delete-texture (texture renderer))
    (cgl:destroy-context (cgl-context renderer))
    (cgl:destroy-pixel-format (pixel-format renderer))))

;;; ================================================================================
;;;  visual-renderer
;;;
(defclass visual-renderer (renderer gfx:shader-environment)
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
   (view-matrix
    :accessor view-matrix)
   (reinit-time
    :initarg :reinit-time
    :accessor reinit-time)
   (render-time
    :accessor render-time)
   (texture-devices
    :initform nil
    :accessor texture-devices)
   (tex-image-table
    :initform (make-hash-table :test #'equal)
    :reader tex-image-table
    :allocation :class)
   (texture-cache
    :accessor texture-cache)
   (texture-cache-flush
    :initform nil
    :accessor texture-cache-flush)
   (gl-canvas
    :initform nil
    :accessor gl-canvas)
   (post-raymarch
    :initform nil
    :accessor post-raymarch)
   (imouse
    :initform (list 0.0 0.0 0.0)
    :accessor imouse)))


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
;;;
;;; for volume / control
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *num-ivolume* 6)
  (defvar *num-icontrol* 10)
  (defvar *visual-volume-function* (lambda (n) (declare (ignore n)) 0.0))
  (defvar *visual-control-function* (lambda (n) (declare (ignore n)) 0.0)))


;;; ===========================================================================


(defmethod initialize-instance :after ((instance visual-renderer) &key)
  (setf (texture-cache instance) (core-video:make-texture-cache (cgl-context instance)
								(pixel-format instance))))


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
	   (need-update nil))
      (setf (texture-devices renderer)
	(loop for device in devices
	      for texture-device = (let ((device (alexandria:ensure-list device)))
				     (init-texture-device renderer (car device) (cdr device)))
	      for target = (ecase (getf (cdr texture-device) :target)
			     (:texture-2d :sampler-2d)
			     (:texture-rectangle :sampler-2d-rect)
			     (:texture-cube-map :sampler-cube)
			     (:texture-buffer :sampler-buffer))
	      for uniform in (gfx::%pipeline-uniforms pipeline)
	      when texture-device
		do (unless (eql (second uniform) target)
		     (setf (second uniform) target)
		     (setf need-update t))
		collect texture-device))
      (when need-update
	(format t "update ichannels [~{~a~^ ~}] for ~a~%"
		(mapcar #'second (subseq (gfx::%pipeline-uniforms pipeline) 0 8))
		(gfx::%pipeline-name pipeline))
	(force-output)
	(gfx::compile-pipeline pipeline)))))

(defun reinit-visual-renderer (renderer options &optional scene-size)
  (with-cgl-context ((cgl-context renderer))
    (when scene-size
      (resize-framebuffer renderer (car scene-size) (second scene-size)))
    (reinit-shader renderer (getf options :shader))
    (reinit-textures renderer options)
    (when-let ((canvas (gl-canvas renderer)))
      (gfx:release canvas))
    (setf (gl-canvas renderer) nil)
    (setf (post-raymarch renderer) (getf options :post-raymarch))
    (when-let ((canvas (getf options :gl-canvas)))
      (setf (gl-canvas renderer) (make-instance canvas :camera (camera renderer)
						:width (width renderer) :height (height renderer)
						:fbo (gfx:make-fbo (width renderer) (height renderer)
								   :target :texture-rectangle
								   :multisample t :use-depth-texture-p t)))
      (gfx:init (gl-canvas renderer)))))



(defun draw-raymarching (renderer time w h)
  (gfx:with-shader (renderer (shader renderer) (gpu-stream renderer))
    #.`(progn ,@(loop for i from 0 below 8
		      collect `(gfx:set-uniform ',(intern (format nil "ICHANNEL~d" i)) ,i))
	      ,@(loop for i from 0 below *num-ivolume*
		      collect `(gfx:set-uniform ',(intern (format nil "IVOLUME~d" i))
						(funcall *visual-volume-function* ,i)))
	      ,@(loop for i from 0 below *num-icontrol*
		      collect `(gfx:set-uniform ',(intern (format nil "ICONTROL~d" i))
						(funcall *visual-control-function* ,i))))
    (when-let ((canvas (gl-canvas renderer)))
      (gl:active-texture :texture8)
      (gl:bind-texture :texture-rectangle (gfx:depth-texture (gfx::fbo canvas)))
      (gfx:set-uniform 'depth-texture 8))
    (gfx:set-uniform 'iglobal-time time)
    (gfx:set-uniform 'itime time)
    (gfx:set-uniform 'iresolution (list w h))
    (gfx:set-uniform 'camera (gfx:camera-position (camera renderer)))
    (gfx:set-uniform 'lookat (gfx:camera-target (camera renderer)))
    (gfx:set-uniform 'projection-matrix (projection-matrix renderer))
    (gfx:set-uniform 'view-matrix (view-matrix renderer))
    (gfx:set-uniform 'imouse (imouse renderer))
    (gl:draw-arrays :triangles 0 6)))

(defun draw-rasterize (renderer canvas)
  (gfx:with-shader (renderer 'gfx::draw-fbo gfx::*fbo-stream*)
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-rectangle (gfx:output-texture (gfx::fbo canvas)))
    (gfx:set-uniform 'ichannel0 0)
    (gl:draw-arrays :triangles 0 (gfx:gpu-stream-length gfx::*fbo-stream*))))


(defun draw-shader (renderer w h update-size)
  (let* ((time (render-time renderer)))
    (when-let ((canvas (gl-canvas renderer)))
      (gfx:with-fbo ((gfx::fbo canvas))
	(setf (gfx:width canvas) w (gfx:height canvas) h)
	(setf (gfx:projection-matrix canvas) (projection-matrix renderer)
	      (gfx:view-matrix canvas) (view-matrix renderer))
	(when update-size
	  (gfx:reshape canvas))
	(gfx:draw canvas)))
    (gl:disable :depth-test)
    (gl:disable :blend)
    (if (post-raymarch renderer)
	(when-let ((canvas (gl-canvas renderer)))
	  (draw-rasterize renderer canvas))
      (draw-raymarching renderer time w h))
    (gl:enable :depth-test)
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (if (post-raymarch renderer)
	(draw-raymarching renderer time w h)
      (when-let ((canvas (gl-canvas renderer)))
	(draw-rasterize renderer canvas)))))


(defun render (renderer update-size)
  (with-cgl-context ((cgl-context renderer))
    (let* ((w (width renderer))
	   (h (height renderer))
	   (gfx:*fbo-stack* (list 0)))
      (gfx:with-fbo ((fbo renderer))
	(gl:viewport 0 0 w h)
	(gl:clear :color-buffer-bit :depth-buffer-bit)
 	(setf (projection-matrix renderer) (kit.math:perspective-matrix 45.0 (/ w h) .1 10000.0)
	      (view-matrix renderer) (gfx:look-at (camera renderer))
	      (render-time renderer) (funcall (reinit-time renderer)))
	(loop for unit in '(:texture0 :texture1 :texture2 :texture3
			    :texture4 :texture5 :texture6 :texture7)
	      for device in (texture-devices renderer)
	      do (gl:active-texture unit)
		 (update-texture-device renderer (car device) (cdr device)))
	(draw-shader renderer w h update-size)
	(loop for unit in '(:texture0 :texture1 :texture2 :texture3
			    :texture4 :texture5 :texture6 :texture7)
	      for device in (texture-devices renderer)
	      for target = (getf (cdr device) :target)
	      do (gl:active-texture unit)
		 (case (car device)
		   (:previous-frame
		    (gl:copy-tex-sub-image-2d target 0 0 0  0 0 w h)))
		 (gl:bind-texture target 0)))) 
    (gl:flush)
    (when (texture-cache-flush renderer)
      (core-video:texture-cache-flush (texture-cache renderer) 0)
      (setf (texture-cache-flush renderer) nil))))

(defmethod release ((renderer visual-renderer))
  (with-cgl-context ((cgl-context renderer))
    (when-let ((canvas (gl-canvas renderer)))
      (gfx:release canvas))
    (loop for device in (texture-devices renderer)
	  do (release-texture-device renderer (car device) (cdr device)))
    (gfx:release-environment renderer))
  (core-video:release-texture-cache (texture-cache renderer))
  (call-next-method))

