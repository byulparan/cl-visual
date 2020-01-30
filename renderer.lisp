(in-package #:shadertoy)

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
   (framebuffer
    :initform nil
    :accessor framebuffer)
   (texture
    :initform nil
    :accessor texture)
   (depthbuffer
    :initform nil
    :accessor depthbuffer)
   (width
    :initarg :width
    :accessor width)
   (height
    :initarg :height
    :accessor height)))

(defmacro with-cgl-context ((cgl-context) &body body)
  `(let* ((current (cgl:get-current-cgl-context)))
     (unwind-protect (progn
		       (cgl:set-current-cgl-context ,cgl-context)
		       ,@body)
       (cgl:set-current-cgl-context current))))

(defun make-framebuffer-from-iosurface (cgl-context framebuffer texture renderbuffer iosurface width height)
  (gl:bind-texture :texture-rectangle texture)
  (io-surface:cgl-teximage-io-surface-2d iosurface cgl-context width height)
  (gl:tex-parameter :texture-rectangle :texture-min-filter :linear)
  (gl:tex-parameter :texture-rectangle :texture-mag-filter :linear)
  (gl:tex-parameter :texture-rectangle :texture-wrap-s :clamp-to-edge)
  (gl:tex-parameter :texture-rectangle :texture-wrap-t :clamp-to-edge)
  (gl:bind-texture :texture-rectangle 0)
  (gl:bind-framebuffer :framebuffer framebuffer)
  (gl:framebuffer-texture-2d :framebuffer :color-attachment0 :texture-rectangle texture 0)
  (gl:bind-renderbuffer :renderbuffer renderbuffer)
  (gl:renderbuffer-storage :renderbuffer :depth-component width height)
  (gl:framebuffer-renderbuffer :framebuffer :depth-attachment :renderbuffer renderbuffer)
  (gl:bind-renderbuffer :renderbuffer 0)
  (unless (eql :framebuffer-complete-oes (gl:check-framebuffer-status-ext :framebuffer))
    (error "can't make framebuffer"))
  (gl:bind-framebuffer :framebuffer 0))

(defmethod resize-framebuffer ((renderer renderer) width height)
  (let* ((cgl-context (cgl-context renderer)))
    (with-cgl-context (cgl-context)
      (when (iosurface renderer)
	(objective-c:release (iosurface renderer)))
      (setf (iosurface renderer) (io-surface:make-io-surface width height))
      (unless (framebuffer renderer)
	(setf (framebuffer renderer) (gl:gen-framebuffer)
	      (texture renderer) (gl:gen-texture)
	      (depthbuffer renderer) (gl:gen-renderbuffer)))
      (make-framebuffer-from-iosurface cgl-context (framebuffer renderer) (texture renderer) (depthbuffer renderer)
				       (iosurface renderer) width height))))


(defmethod initialize-instance :around ((instance renderer) &rest initargs)
  (let* ((%pixel-format (cgl:make-cgl-pixel-format :core-profile (getf initargs :core-profile)))
	 (%cgl-context (cgl:make-cgl-context %pixel-format)))
    (with-slots (cgl-context pixel-format) instance
      (setf cgl-context %cgl-context
	    pixel-format %pixel-format))
    (call-next-method)))

(defmethod destroy ((renderer renderer))
  (with-cgl-context ((cgl-context renderer))
    (gl:delete-textures (list (texture renderer)))
    (gl:delete-renderbuffers (list (depthbuffer renderer)))
    (gl:delete-framebuffers (list (framebuffer renderer)))
    (objective-c:release (iosurface renderer))
    (cgl:destroy-cgl-context (cgl-context renderer))
    (cgl:destroy-cgl-pixel-format (pixel-format renderer))))


(defmacro with-renderer ((renderer) &body body)
  `(with-cgl-context ((cgl-context ,renderer))
     (gl:bind-framebuffer :framebuffer (framebuffer ,renderer))
     ,@body
     (gl:flush)
     (gl:bind-framebuffer :framebuffer 0)))

;;; ================================================================================
;;;  shadertoy-renderer
;;;
(defclass shadertoy-renderer (renderer)
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
   (mouse-pos
    :initform (list 0.0 0.0 0.0)
    :accessor mouse-pos)
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
    :allocation :class)))

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

(defun clear-shader (renderer)
  (loop for (name shader-spec) on (gfx::shaders renderer) by #'cddr
	do (progn
	     name ;; ignore name 
	     (destructuring-bind (build-time prog (vs fs))
		 shader-spec
	       (declare (ignore build-time))
	       (gl:detach-shader prog vs)
	       (gl:delete-shader vs)
	       (gl:detach-shader prog fs)
	       (gl:delete-shader fs)
	       (gl:delete-program prog))))
  (setf (gfx::shaders renderer) nil))

(defun reinit-shader-toy (renderer options &optional scene-size)
  (with-cgl-context ((cgl-context renderer))
    (when scene-size
      (setf (width renderer) (car scene-size)
	    (height renderer) (second scene-size))
      (when (iosurface renderer) (objective-c:release (iosurface renderer)))
      (setf (iosurface renderer) (io-surface:make-io-surface (width renderer) (height renderer)))
      (resize-framebuffer renderer (width renderer) (height renderer)))
    (clear-shader renderer)
    (setf (shader renderer) (getf options :shader))
    (loop for src in (texture-srcs renderer)
	  do (destroy-texture-src renderer (getf (alexandria:ensure-list src) :src) src (getf options :textures)))
    (let* ((srcs (getf options :textures)))
      (setf (texture-srcs renderer) (loop for src in srcs
					  collect (let ((src (alexandria:ensure-list src)))
						    (process-texture-src renderer (car src) src))))
      (let* ((pipeline (gethash (shader renderer) gfx::*all-pipeline-table*))
	     (texture-types (texture-types (texture-srcs renderer))))
	(unless (equal
		 (mapcar #'second
			 (subseq (gfx::%pipeline-uniforms pipeline) 0 (length texture-types)))
		 texture-types)
	  (loop for tex in texture-types
		for uniforms in (gfx::%pipeline-uniforms pipeline)
		for i from 0 
		do (unless (eql (second uniforms) tex)
		     (gl:delete-textures (list (nth i (textures-toy renderer))))
		     (setf (nth i (textures-toy renderer)) (gl:gen-texture))
		     (setf (second (nth i (gfx::%pipeline-uniforms pipeline))) tex)))
	  (gfx::update-source pipeline)))
      (loop for src in (texture-srcs renderer)
	    for tex-id in (textures-toy renderer)
	    for unit in '(:texture0 :texture1 :texture2 :texture3
			  :texture4 :texture5 :texture6 :texture7)
	    do (gl:active-texture unit)
	       (init-texture-src renderer tex-id (getf src :src) src)))))

(defmethod render ((renderer shadertoy-renderer))
  (with-renderer (renderer)
    (let* ((w (width renderer))
	   (h (height renderer)))
      (gl:viewport 0 0 w h)
      (gl:clear :color-buffer-bit)
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
	  (apply (shader renderer) renderer `(:triangles 0 6 ,(gpu-stream renderer)
					      :ichannel0 0
					      :ichannel1 1
					      :ichannel2 2
					      :ichannel3 3
					      :ichannel4 4
					      :ichannel5 5
					      :ichannel6 6
					      :ichannel7 7
					      :iglobal-time ,time
					      :itime ,time
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
							    (gfx::center-z (camera renderer)))))))
      (loop for unit in '(:texture0 :texture1 :texture2 :texture3
			  :texture4 :texture5 :texture6 :texture7)
	    for src in (texture-srcs renderer)
	    do (gl:active-texture unit)
	       (case (getf src :src)
		 (:previous-frame (gl:copy-tex-image-2d :texture-2d 0 :rgba8 0 0 w h 0))
		 (:syphon (#/release (getf src :syphon-image))))
	       (gl:bind-texture (getf src :target) 0)))))

(defmethod destroy ((renderer shadertoy-renderer))
  (with-cgl-context ((cgl-context renderer))
    (loop for src in (texture-srcs renderer)
	  do (destroy-texture-src renderer (getf src :src) src nil))
    (call-next-method)))

