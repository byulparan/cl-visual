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

(defmethod destroy ((renderer renderer))
  (with-cgl-context ((cgl-context renderer))
    (gfx:cleanup-context renderer)
    (gfx:cleanup-fbo (fbo renderer))
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
   (textures-toy
    :initarg :textures-toy
    :initform nil
    :accessor textures-toy)
   (texture-srcs
    :initarg :texture-srcs
    :initform nil
    :accessor texture-srcs)
   (tex-image-table
    :initform (make-hash-table :test #'equal)
    :reader tex-image-table
    :allocation :class)
   (gl-canvas
    :initform nil
    :accessor gl-canvas)))

(defun default-gl-tex-parameter (filter wrap)
  (let ((min-filter filter))
    (when (eql filter :mipmap)
      (setf min-filter :linear-mipmap-linear
	    filter :linear))
    (gl:tex-parameter :texture-2d :texture-mag-filter filter)
    (gl:tex-parameter :texture-2d :texture-min-filter min-filter)
    (gl:tex-parameter :texture-2d :texture-wrap-s wrap)
    (gl:tex-parameter :texture-2d :texture-wrap-t wrap)))



;;; ===========================================================================
;;;
;;; for texture source
;;;

(defgeneric process-texture-src (view src texture-src)
  (:method (view src texture-src)
    (error "You should implementation this method ~a ~a" src texture-src)))

(defgeneric init-texture-src (view tex-id src texture-src)
  (:method (view tex-id src texture-src)
    (declare (ignore view src))
    (gl:bind-texture :texture-2d tex-id)
    (default-gl-tex-parameter (getf texture-src :filter) (getf texture-src :wrap))
    (gl:bind-texture :texture-2d 0)))

(defgeneric update-texture-src (view src texture-src)
  (:method (view src texture-src)
    (declare (ignore view src texture-src))))

(defgeneric destroy-texture-src (view src texture-src new-texture-srcs)
  (:method (view src texture-src new-texture-srcs)
    (declare (ignore view src texture-src  new-texture-srcs))))


;;; ===========================================================================
(defun texture-types (texture-srcs)
  (mapcar (lambda (src) (if (eql :texture-2d (getf src :target)) :sampler-2d :sampler-2d-rect))
	  texture-srcs))

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

(defun reinit-visual-renderer (renderer options &optional scene-size)
  (with-cgl-context ((cgl-context renderer))
    (when scene-size
      (setf (width renderer) (car scene-size)
	    (height renderer) (second scene-size))
      (resize-framebuffer renderer (width renderer) (height renderer)))
    (reinit-shader renderer (getf options :shader))
    (loop for src in (texture-srcs renderer)
	  do (destroy-texture-src renderer (getf (alexandria:ensure-list src) :src) src (getf options :textures)))
    (let* ((srcs (getf options :textures)))
      (setf (texture-srcs renderer) (loop for src in srcs
					  collect (let ((src (alexandria:ensure-list src)))
						    (process-texture-src renderer (car src) src))))
      (let* ((pipeline (gethash (shader renderer) gfx::*all-pipeline-table*))
	     (texture-types (texture-types (texture-srcs renderer))))
	(unless (equal (mapcar #'second (subseq (gfx::%pipeline-uniforms pipeline) 0 (length texture-types)))
		       texture-types)
	  (loop for tex in texture-types
		for uniforms in (gfx::%pipeline-uniforms pipeline)
		for i from 0 
		do (unless (eql (second uniforms) tex)
		     (gl:delete-texture (nth i (textures-toy renderer)))
		     (setf (nth i (textures-toy renderer)) (gl:gen-texture))
		     (setf (second (nth i (gfx::%pipeline-uniforms pipeline))) tex)))
	  (gfx::update-source pipeline)))
      (loop for src in (texture-srcs renderer)
	    for tex-id in (textures-toy renderer)
	    for unit in '(:texture0 :texture1 :texture2 :texture3
			  :texture4 :texture5 :texture6 :texture7)
	    do (gl:active-texture unit)
	       (init-texture-src renderer tex-id (getf src :src) src))
      (when-let ((canvas (gl-canvas renderer)))
	(setf (gfx:width canvas) (width renderer)
	      (gfx:height canvas) (height renderer))
	(gfx:shutdown canvas)
	(gfx:cleanup-context canvas))
      (setf (gl-canvas renderer) nil)
      (when-let ((canvas (getf options :gl-canvas)))
	(setf (gl-canvas renderer) (make-instance canvas :camera (camera renderer)
						  :width (width renderer) :height (height renderer)))
	(gfx:init (gl-canvas renderer))))))

(defmacro gfx::define-shader (name &body body)
  (flet ((in (symb)
  	   (intern (string-upcase symb) :cl-visual)))
    `(gfx:defpipeline (,name :version 330)
	 ((,(in 'ichannel0) :sampler-2d)
	  (,(in 'ichannel1) :sampler-2d)
	  (,(in 'ichannel2) :sampler-2d)
	  (,(in 'ichannel3) :sampler-2d)
	  (,(in 'ichannel4) :sampler-2d)
	  (,(in 'ichannel5) :sampler-2d)
	  (,(in 'ichannel6) :sampler-2d)
	  (,(in 'ichannel7) :sampler-2d)
	  (,(in 'iglobal-time) :float)
	  (,(in 'itime) :float)
	  ,@(loop for i from 0 below 6
		  collect (list (in (intern (format nil "IVOLUME~d" i))) :float))
	  ,@(loop for i from 0 below 10
		  collect (list (in (intern (format nil "ICONTROL~d" i))) :float))
	  (,(in 'iresolution) :vec2)
	  (,(in 'camera) :vec3)
	  (,(in 'lookat) :vec3)
	  (,(in 'projection-matrix) :mat4)
	  (,(in 'modelview-matrix) :mat4))
       (:vertex ((,(in 'pos) :vec2))
		(values
		 (v! ,(in 'pos) 0.0 1.0)
		 ,(in 'pos)))
       (:fragment ((vfuv :vec2))
    		  (progn ,@body)))))

(defun render (renderer)
  (with-cgl-context ((cgl-context renderer))
    (let* ((w (width renderer))
	   (h (height renderer))
	   (draw-fbo (if (gl-canvas renderer) (fbo renderer)
		       (gfx::output-fbo (fbo renderer)))))
      (gfx:with-fbo (draw-fbo)
	(gl:viewport 0 0 w h)
	(gl:enable :depth-test)
	(gl:clear :color-buffer-bit :depth-buffer-bit)
	(loop for tex-id in (textures-toy renderer)
	      for unit in '(:texture0 :texture1 :texture2 :texture3
			    :texture4 :texture5 :texture6 :texture7)
	      for src in (texture-srcs renderer)
	      do (gl:active-texture unit)
		 (unless (eql (getf src :src) :syphon)
		   (gl:bind-texture (getf src :target) tex-id))
		 (update-texture-src renderer (getf src :src) src))
	(let ((time (funcall (reinit-time renderer))))
	  (when (shader renderer)
	    (setf (projection-matrix renderer) (kit.math:perspective-matrix 45.0 (/ w h) .1 10000.0)
		  (modelview-matrix renderer) (gfx:eval-camera (camera renderer)))
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
						:modelview-matrix ,(modelview-matrix renderer))))
	  (when-let ((canvas (gl-canvas renderer)))
	    (setf (gfx:width canvas) w (gfx:height canvas) h)
	    (setf (gfx:projection-matrix canvas) (projection-matrix renderer)
		  (gfx:modelview-matrix canvas) (modelview-matrix renderer))
	    (gfx:draw canvas)))
	(gl:disable :depth-test))
      (gfx:with-fbo ((gfx::output-fbo (fbo renderer)))
	(loop for unit in '(:texture0 :texture1 :texture2 :texture3
			    :texture4 :texture5 :texture6 :texture7)
	      for src in (texture-srcs renderer)
	      do (gl:active-texture unit)
		 (case (getf src :src)
		   (:previous-frame (gl:copy-tex-image-2d :texture-2d 0 :rgba8 0 0 w h 0))
		   (:syphon (ns:release (getf src :syphon-image))))
		 (gl:bind-texture (getf src :target) 0))))
    (gl:flush)))

(defmethod destroy ((renderer visual-renderer))
  (with-cgl-context ((cgl-context renderer))
    (loop for src in (texture-srcs renderer)
	  do (destroy-texture-src renderer (getf src :src) src nil))
    (call-next-method)))

