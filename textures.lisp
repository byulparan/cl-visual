(in-package :cl-visual)

(defmacro tex (key)
  `(getf texture-device ,key))


;; previous frame
(defmethod init-texture-device (view (device (eql :previous-frame)) texture-device)
  (declare (ignorable view texture-device))
  (let ((texture (gl:gen-texture))
	(target :texture-rectangle)
	(filter (or (getf texture-device :filter) :linear))
	(wrap :clamp-to-edge))
    (gl:bind-texture  target texture)
    (gl:tex-parameter target :texture-mag-filter filter)
    (gl:tex-parameter target :texture-min-filter filter)
    (gl:tex-parameter target :texture-wrap-s wrap)
    (gl:tex-parameter target :texture-wrap-t wrap)
    (gl:bind-texture  target 0)
    (list device :tex-id texture :target target)))

(defmethod update-texture-device (view (device (eql :previous-frame)) texture-device)
  (declare (ignorable view device))
  (gl:bind-texture (tex :target) (tex :tex-id)))

(defmethod release-texture-device (view (device (eql :previous-frame)) texture-device)
  (declare (ignorable view device))
  (gl:delete-texture (tex :tex-id)))


;;; 
;;; image
;;;
(defmethod init-texture-device (view (device string) texture-device)
  (declare (ignorable view))
  (let* ((full-path (uiop:truenamize device)))
    (assert full-path nil "~s : can't find image file" device)
    (setf full-path (namestring full-path))
    (let* ((image (gethash full-path (tex-image-table (renderer *visual-canvas*)))))
      (unless image
	(setf image (cg:load-image full-path)
	      (gethash full-path (tex-image-table (renderer *visual-canvas*))) image))
      (init-texture-device view :image (append (list :src image) texture-device)))))

(defmethod init-texture-device (view (device (eql :image)) texture-device)
  (declare (ignorable view))
  (let* ((image (tex :src))
	 (texture (gl:gen-texture))
	 (target :texture-2d)
	 (format (ecase (cg:image-bits-per-pixel image)
		   (32 (list :rgba8 :rgba))
		   (24 (list :rgb8 :rgb))
		   (8 (list :red :red))))
	 (w (cg:image-width image))
	 (h (cg:image-height image))
	 (filter (or (getf texture-device :filter) :mipmap))
	 (use-mipmap (eql filter :mipmap))
	 (wrap (or (getf texture-device :wrap) :repeat)))
    (gl:bind-texture target texture)
    (gl:tex-image-2d target 0 (first format) w h 0 (second format) :unsigned-byte
		     (cg:image-data image))
    (gl:tex-parameter target :texture-mag-filter (if use-mipmap :linear filter))
    (gl:tex-parameter target :texture-min-filter (if use-mipmap :linear-mipmap-linear
						   filter))
    (gl:tex-parameter target :texture-wrap-s wrap)
    (gl:tex-parameter target :texture-wrap-t wrap)
    (when use-mipmap (gl:generate-mipmap target))
    (gl:bind-texture  target 0)
    (list device :tex-id texture :target target)))


(defmethod update-texture-device (view (device (eql :image)) texture-device)
  (declare (ignorable view device))
  (gl:bind-texture (tex :target) (tex :tex-id)))

(defmethod release-texture-device (view (device (eql :image)) texture-device)
  (declare (ignorable view device))
  (gl:delete-texture (tex :tex-id)))



;;; 
;;; cubemap
;;;
(defmethod init-texture-device (view (device (eql :cubemap)) texture-device)
  (declare (ignorable view))
  (assert (= (length (tex :src)) 6) nil "length of cubemap's src should be 6")
  (flet ((load-image (src)
	   (let* ((full-path (uiop:truenamize src)))
	         (assert full-path nil "~s : can't find image file" device)
	     (setf full-path (namestring full-path))
	     (let* ((image (gethash full-path (tex-image-table (renderer *visual-canvas*)))))
	       (unless image
		 (setf image (cg:load-image full-path)
		       (gethash full-path (tex-image-table (renderer *visual-canvas*))) image))
	       image))))
    (let* ((images (mapcar #'load-image (tex :src)))
	   (texture (gl:gen-texture))
	   (target :texture-cube-map)
	   (format (ecase (cg:image-bits-per-pixel (car images))
		     (32 (list :rgba8 :rgba))
		     (24 (list :rgb8 :rgb))
		     (8 (list :red :red))))
	   (w (cg:image-width (car images)))
	   (h (cg:image-height (car images)))
	   (filter (or (getf texture-device :filter) :mipmap))
	   (use-mipmap (eql filter :mipmap))
	   (wrap :clamp-to-edge))
      (gl:bind-texture target texture)
      (loop for face in '(:texture-cube-map-positive-x
			  :texture-cube-map-negative-x
			  :texture-cube-map-positive-y
			  :texture-cube-map-negative-y
			  :texture-cube-map-positive-z
			  :texture-cube-map-negative-z)
	    for i from 0
	    do (gl:tex-image-2d face 0 (first format) w h 0 (second format) :unsigned-byte
				(cg:image-data (nth i images))))
      (gl:tex-parameter target :texture-mag-filter (if use-mipmap :linear filter))
      (gl:tex-parameter target :texture-min-filter (if use-mipmap :linear-mipmap-linear filter))
      (gl:tex-parameter target :texture-wrap-s wrap)
      (gl:tex-parameter target :texture-wrap-t wrap)
      (gl:tex-parameter target :texture-wrap-r wrap)
      (when use-mipmap (gl:generate-mipmap target))
      (gl:bind-texture target 0)
      (list device :tex-id texture :target target))))


(defmethod update-texture-device (view (device (eql :cubemap)) texture-device)
  (declare (ignorable view device))
  (gl:bind-texture (tex :target) (tex :tex-id)))

(defmethod release-texture-device (view (device (eql :cubemap)) texture-device)
  (declare (ignorable view device))
  (gl:delete-texture (tex :tex-id)))



;;; 
;;; bitmap-context
;;;
(defmethod init-texture-device (view (device (eql :bitmap-context)) texture-device)
  (declare (ignorable view))
  (let* ((texture (gl:gen-texture))
	 (target :texture-rectangle)
	 (filter (or (getf texture-device :filter) :linear))
	 (wrap :clamp-to-edge)
	 (fixed-size (tex :size))
	 (fixed-context (tex :context))
	 (context (if fixed-context fixed-context (apply #'cg:make-bitmap-context
							 (if fixed-size fixed-size (list (width view) (height view))))))
	 (object (make-instance (tex :src) :context context)))
    (gfx:init object)
    (gl:bind-texture target texture)
    (gl:tex-parameter target :texture-mag-filter filter)
    (gl:tex-parameter target :texture-min-filter filter)
    (gl:tex-parameter target :texture-wrap-s wrap)
    (gl:tex-parameter target :texture-wrap-t wrap)
    (gl:bind-texture target 0)
    (list device 
	  :fixed-size fixed-size
	  :fixed-context fixed-context
	  :object object
	  :tex-id texture :target target)))

(defmethod update-texture-device (view (device (eql :bitmap-context)) texture-device)
  (declare (ignorable view device))
  (let* ((object (tex :object)))
    (when (and (not (tex :fixed-size))
	       (not (tex :fixed-context))
	       (or 
		(/= (gfx:width object) (width view))
		(/= (gfx:height object) (height view))))
      (cg:release-context (gfx:context object))
      (setf (gfx:context object) (cg:make-bitmap-context (width view) (height view)))
      (gfx:reshape object))
    (gfx:draw object)
    (gl:bind-texture (tex :target) (tex :tex-id))
    (gl:tex-image-2d (tex :target) 0 :rgba8 (gfx:width object) (gfx:height object) 0 :rgba :unsigned-byte
		     (cg:context-data (gfx:context object)))))

(defmethod release-texture-device (view (device (eql :bitmap-context)) texture-device)
  (declare (ignorable view device))
  (let* ((object (tex :object)))
    (gfx:release object)
    (when (not (tex :fixed-context))
      (cg:release-context (gfx:context object))))
  (gl:delete-texture (tex :tex-id)))


;;;
;;; av-player
;;;
(defmethod init-texture-device (view (device av:player) texture-device)
  (declare (ignorable view texture-device))
  (let* ((texture-cache (core-video:make-texture-cache (cgl-context view)
						       (pixel-format view)))
	 (target (tex :target)))
    (list device
	  :texture-cache texture-cache
	  :target (if target target :texture-rectangle)
	  :info (tex :info))))

(defmethod update-texture-device (view (device av:player) texture-device)
  (declare (ignorable view))
  (av:with-texture-cache (device (tex :texture-cache) width height)
    (when (tex :info)
      (uiop:println (list :player-frame-size width height)))))

(defmethod release-texture-device (view (device av:player) texture-device)
  (declare (ignorable view device))
  (core-video:release-texture-cache (tex :texture-cache)))



;;; 
;;; av-capture
;;; 
(defmethod init-texture-device (view (device (eql :live-frame)) texture-device)
  (declare (ignorable view))
  (let* ((index (tex :src))
	 (size (tex :size)))
    (unless index (setf index 0))
    (let* ((capture (av:make-camera-capture index :request-size size))
	   (texture-cache (core-video:make-texture-cache (cgl-context view)
							 (pixel-format view))))
      (av:start-capture capture)
      (list capture
	    :release-p t
	    :texture-cache texture-cache
	    :target :texture-rectangle
	    :info (tex :info)))))

(defmethod init-texture-device (view (device (eql :screen-frame)) texture-device)
  (declare (ignorable view))
  (let* ((rect (tex :src))
	 (fps (tex :fps))
	 (size (tex :size)))
    (let* ((capture (av:make-screen-capture :crop-rect (when rect (apply #'ns:make-rect rect))
					    :min-frame-duration (if fps fps 60)
					    :request-size size))
	   (texture-cache (core-video:make-texture-cache (cgl-context view)
	   						 (pixel-format view))))
      (ns:queue-for-event-loop (lambda () (av:start-capture capture)))
      (list capture
	    :release-p t
	    :texture-cache texture-cache
	    :target :texture-rectangle
	    :info (tex :info)))))


(defmethod init-texture-device (view (device av:capture) texture-device)
  (declare (ignorable view texture-device))
  (let* ((texture-cache (core-video:make-texture-cache (cgl-context view)
						       (pixel-format view))))
    (list device
	  :release-p nil
	  :texture-cache texture-cache
	  :target :texture-rectangle
	  :info (tex :info))))

(defmethod update-texture-device (view (device av:capture) texture-device)
  (declare (ignorable view))
  (av:with-texture-cache (device (tex :texture-cache) width height)
    (when (tex :info)
      (uiop:println (list :capture-frame-size width height)))))

(defmethod release-texture-device (view (device av:capture) texture-device)
  (declare (ignorable view))
  (when (tex :release-p)
    (ns:queue-for-event-loop
     (lambda ()
       (av:stop-capture device)
       (av:release-capture device))))
  (core-video:release-texture-cache (tex :texture-cache)))

;;; 
;;; syphon
;;; 
(defmethod init-texture-device (view (device (eql :syphon)) texture-device)
  (flet ((parse-src (name)
	   (let* ((pos (position #\: name))
		  (app (subseq name 0 pos))
		  (name (if pos (subseq name (+ pos 1)) nil)))
	     (list app (if (zerop (length name)) nil name)))))
    (let* ((dict (parse-src (tex :src))))
      (list device
	    :app (car dict)
	    :name (second dict)
	    :size (list 0 0)
	    :syphon-client nil
	    :target :texture-rectangle))))

(defmethod update-texture-device (view (device (eql :syphon)) texture-device)
  (declare (ignore device))
  (unless (tex :syphon-client)
    (when-let* ((descriptor (syphon:get-server (tex :app) (tex :name)))
		(client (syphon:make-client descriptor (cgl-context view))))
      (setf (tex :syphon-client) client)))
  (when-let ((syphon (tex :syphon-client)))
    (let* ((image (syphon:new-frame-image syphon)))
      (unless (cffi:null-pointer-p image)
	(ns:autorelease image)
	(let* ((size (syphon:texture-size image))
	       (w (ns:size-width size))
	       (h (ns:size-height size))
	       (orig-size (tex :size))
	       (name (syphon:texture-name image)))
	  (unless (and (= w (car orig-size))
		       (= h (second orig-size)))
	    (format t "~&<Syphon-\"~a~@[:~a~]\"> image-size: ~a, ~a image-name: ~a~%"
		    (tex :app) (tex :name) w h name)
	    (setf (tex :size) (list w h)))
	  (gl:bind-texture (tex :target) name))))))

(defmethod release-texture-device (view (device (eql :syphon)) texture-device)
  (declare (ignore view device))
  (when-let ((syphon (tex :syphon-client)))
    (syphon:stop-client syphon)
    (ns:release syphon)))

;;; simple-array singloe-float
(defmethod init-texture-device (view (device #+sbcl sb-kernel::simple-array-single-float
					     #+ccl ccl::simple-short-float-vector
					     #+ecl vector
					     #+lispworks vector)
				texture-device)
  (declare (ignorable view texture-device))
  (let* ((texture (gl:gen-texture))
	 (target :texture-rectangle)
	 (filter (or (getf texture-device :filter) :nearest))
	 (wrap :clamp-to-edge))
    (gl:bind-texture  target texture)
    (gl:tex-parameter target :texture-min-filter filter)
    (gl:tex-parameter target :texture-mag-filter filter)
    (gl:tex-parameter target :texture-wrap-s wrap)
    (gl:tex-parameter target :texture-wrap-t wrap)
    (gl:bind-texture  target 0)
    (list device
	  :tex-id texture :target target)))

(defmethod update-texture-device (view (device #+sbcl sb-kernel::simple-array-single-float
					        #+ccl ccl::simple-short-float-vector
						#+lispworks vector
						#+ecl vector)
				  texture-device)
  (declare (ignore view))
  (gl:bind-texture (tex :target) (tex :tex-id))
  (cffi:with-pointer-to-vector-data (ptr device)
    (gl:tex-image-2d (tex :target) 0 :r32f (length device) 1 0 :red :float ptr)))

(defmethod release-texture-device (view (device #+sbcl sb-kernel::simple-array-single-float
						#+ccl ccl::simple-short-float-vector
						#+lispworks vector
						#+ecl vector)
				   texture-device)
  (declare (ignore view device))
  (gl:delete-texture (tex :tex-id)))


;;
(defvar *io-surface-table* (make-hash-table))

;;; io-surface
(defmethod init-texture-device (view (device (eql :io-surface)) texture-device)
  (let* ((src (tex :src))
	 (io-surface (gethash src *io-surface-table*)))
    (when io-surface
      (let* ((texture (gl:gen-texture))
	     (target :texture-rectangle)
	     (width (io-surface:width io-surface))
	     (height (io-surface:height io-surface)))
	(gl:bind-texture target texture)
	(cgl:tex-image-io-surface-2d (cgl-context view) target
				     :rgba width height :bgra
				     :unsigned-int-8-8-8-8-rev io-surface 0)
	(gl:bind-texture target 0)
	(list device
	      :src (tex :src)
	      :tex-id texture
	      :target target
	      :width width
	      :height height)))))

(defmethod update-texture-device (view (device (eql :io-surface)) texture-device)
  (declare (ignore device))
  (let* ((io-surface (gethash (tex :src) *io-surface-table*))
	 (width (io-surface:width io-surface))
	 (height (io-surface:height io-surface)))
    (when (or (/= (tex :width) width)
	      (/= (tex :height) height))
      (setf (tex :width) width
	    (tex :height) height)
      (gl:bind-texture (tex :target) (tex :tex-id))
      (cgl:tex-image-io-surface-2d (cgl-context view) (tex :target) :rgba
				   width height :bgra :unsigned-int-8-8-8-8-rev io-surface 0)
      (gl:bind-texture (tex :target) 0))
    (gl:bind-texture (tex :target) (tex :tex-id))))

(defmethod release-texture-device (view (device (eql :io-surface)) texture-device)
  (declare (ignore view device))
  (gl:delete-texture (tex :tex-id)))



;;; gl-canvas
(defmethod init-texture-device (view (device (eql :gl-canvas)) texture-device)
  (declare (ignorable device))
  (let* ((core-profile (tex :core-profile))
	 (texture (gl:gen-texture))
	 (target :texture-rectangle)
	 (fixed-size (tex :size))
	 (width (if fixed-size (first (tex :size)) (width view)))
	 (height (if fixed-size (second (tex :size)) (height view)))
	 (renderer (make-instance 'renderer :width width :height height :core-profile core-profile))
	 (fbo nil)
	 (gl-canvas (make-instance (tex :src) :width width :height height
				   :camera (if (tex :shared-camera) (camera (renderer *visual-canvas*))
					     (make-instance 'gfx:camera))))
	 (output (tex :output)))
    (resize-framebuffer renderer width height)
    (when output
      (setf (gethash output *io-surface-table*) (iosurface renderer)))
    (with-cgl-context ((cgl-context renderer))
      (let ((fbo (if (tex :multisample) (fbo renderer) (gfx::output-fbo (fbo renderer)))))
	(gfx:with-fbo (fbo)
	  (gfx:init gl-canvas))))
    (gl:bind-texture target texture)
    (cgl:tex-image-io-surface-2d (cgl-context view) target
				 :rgba width height :bgra
				 :unsigned-int-8-8-8-8-rev (iosurface renderer) 0)
    (gl:bind-texture target 0)
    (list device
	  :tex-id texture
	  :target target
	  :renderer renderer
	  :multisample (tex :multisample)
	  :gl-canvas gl-canvas
	  :fixed-size fixed-size
	  :output output)))

(defmethod update-texture-device (view (device (eql :gl-canvas)) texture-device)
  (declare (ignore device))
  (let* ((width (width view))
	 (height (height view))
	 (renderer (tex :renderer))
	 (canvas (tex :gl-canvas))
	 (output (tex :output))
	 (resize nil))
    (when (and (not (tex :fixed-size))
	       (or (/= width (width renderer))
		   (/= height (height renderer))))
      (resize-framebuffer renderer width height)
      (when output
	(setf (gethash output *io-surface-table*) (iosurface renderer)))
      (setf (gfx:width canvas) width (gfx:height canvas) height)
      (gl:bind-texture (tex :target) (tex :tex-id))
      (cgl:tex-image-io-surface-2d (cgl-context view) (tex :target)
				   :rgba width height :bgra
				   :unsigned-int-8-8-8-8-rev (iosurface renderer) 0)
      (gl:bind-texture (tex :target) 0)
      (setf resize t))
    (gl:bind-texture (tex :target) (tex :tex-id))
    (with-cgl-context ((cgl-context renderer))
      (let* ((fbo (if (tex :multisample) (fbo renderer) (gfx::output-fbo (fbo renderer)))))
	(gfx:with-fbo (fbo)
	  (setf (gfx:projection-matrix canvas) (kit.math:perspective-matrix 45.0 (/ width height) .1 10000.0)
		(gfx:modelview-matrix canvas) (gfx:eval-camera (gfx:camera canvas)))
	  (when resize
	    (gfx:reshape canvas))
	  (gfx:draw canvas)))
      (gl:flush))))


(defmethod release-texture-device (view (device (eql :gl-canvas)) texture-device)
  (declare (ignore view device))
  (let* ((renderer (tex :renderer))
	 (canvas (tex :gl-canvas))
	 (output (tex :output)))
    (with-cgl-context ((cgl-context renderer))
      (let* ((fbo (if (tex :multisample) (fbo renderer) (gfx::output-fbo (fbo renderer)))))
	(gfx:with-fbo (fbo)
	  (gfx:release canvas))))
    (when output
      (remhash output *io-surface-table*))
    (release renderer)
    (gl:delete-texture (tex :tex-id))))


;;; shader
(defclass shader-surface (gfx:gl-canvas)
  ((renderer :initarg :renderer :reader renderer)
   (shader :initarg :shader :reader shader)
   (texture-devices :initarg :texture-devices :accessor texture-devices)
   (gl-canvas :initarg :gl-canvas :accessor gl-canvas)
   (user-fn :initarg :user-fn :reader user-fn)
   (resize-p :initform nil :accessor resize-p)))

(defmethod gfx:init ((view shader-surface))
  (let* ((devices (texture-devices view)))
    (let* ((pipeline (gethash (shader view) gfx::*all-pipeline-table*))
	   (need-update nil))
      (setf (texture-devices view)
	(loop for device in devices
	      for texture-device = (let ((device (alexandria:ensure-list device)))
				     (init-texture-device view (car device) (cdr device)))
	      for target = (ecase (getf (cdr texture-device) :target)
			     (:texture-2d :sampler-2d)
			     (:texture-rectangle :sampler-2d-rect))
	      for uniform in (gfx::%pipeline-uniforms pipeline)
	      when texture-device
		do (unless (eql (second uniform) target)
		     (setf (second uniform) target)
		     (setf need-update t))
	      collect texture-device))
      (when need-update
	(format t "update ichannel type for ~a~%" (gfx::%pipeline-name pipeline))
	(force-output)
	(gfx::compile-pipeline pipeline))
      (when (gl-canvas view)
	(setf (gl-canvas view) (make-instance (gl-canvas view) :width (gfx:width view) :height (gfx:height view)
					      :camera (gfx:camera view)))
	(gfx:init (gl-canvas view))))))

(defmethod gfx:draw ((view shader-surface))
  (let* ((w (gfx:width view))
	 (h (gfx:height view))
	 (time (render-time (renderer *visual-canvas*))))
    (gl:viewport 0 0 w h)
    (gl:clear-color .0 .0 .0 1.0)
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (when (user-fn view)
      (funcall (user-fn view)))
    (setf (gfx:projection-matrix view) (kit.math:perspective-matrix 45.0 (/ w h) .1 10000.0)
	  (gfx:modelview-matrix view) (gfx:eval-camera (gfx:camera view)))
    (loop for unit in '(:texture0 :texture1 :texture2 :texture3
			:texture4 :texture5 :texture6 :texture7)
	  for device in (texture-devices view)
	  do (gl:active-texture unit)
	     (update-texture-device view (car device) (cdr device)))
    (gl:enable :depth-test)
    (gfx:with-shader (view (shader view) (gpu-stream (renderer *visual-canvas*)) program)
      #.`(progn ,@(loop for i from 0 below 8
			collect `(gfx:set-uniform ',(intern (format nil "ICHANNEL~d" i)) ,i))
		,@(loop for i from 0 below *num-ivolume*
			collect `(gfx:set-uniform ',(intern (format nil "IVOLUME~d" i))
						  (funcall *visual-volume-function* ,i)))
		,@(loop for i from 0 below *num-icontrol*
			collect `(gfx:set-uniform ',(intern (format nil "ICONTROL~d" i))
						  (funcall *visual-control-function* ,i))))
      (gfx:set-uniform 'iglobal-time time)
      (gfx:set-uniform 'itime time)
      (gfx:set-uniform 'iresolution (list w h))
      (gfx:set-uniform 'camera (list
				(gfx::eye-x (gfx:camera view))
				(gfx::eye-y (gfx:camera view))
				(gfx::eye-z (gfx:camera view))))
      (gfx:set-uniform 'lookat (list
				(gfx::center-x (gfx:camera view))
				(gfx::center-y (gfx:camera view))
				(gfx::center-z (gfx:camera view))))
      (gfx:set-uniform 'projection-matrix (gfx:projection-matrix view))
      (gfx:set-uniform 'modelview-matrix (gfx:modelview-matrix view))
      (gfx:set-uniform 'imouse (imouse (renderer *visual-canvas*)))
      (gl:draw-arrays :triangles 0 6))
    (gl:disable :depth-test)
    (when (gl-canvas view)
      (setf (gfx:width (gl-canvas view)) w (gfx:height (gl-canvas view)) h)
      (setf (gfx:projection-matrix (gl-canvas view)) (gfx:projection-matrix view)
	    (gfx:modelview-matrix (gl-canvas view)) (gfx:modelview-matrix view))
      (when (resize-p view)
	(gfx:reshape (gl-canvas view))
	(setf (resize-p view) nil))
      (gfx:draw (gl-canvas view)))))

(defmethod gfx:release ((view shader-surface))
  (when (gl-canvas view)
    (gfx:release (gl-canvas view)))
  (loop for device in (texture-devices view)
	do (release-texture-device view (car device) (cdr device))))


(defmethod cgl-context ((view shader-surface))
  (cgl-context (renderer view)))

(defmethod pixel-format ((view shader-surface))
  (pixel-format (renderer view)))


(defmethod width ((view shader-surface))
  (gfx:width view))

(defmethod height ((view shader-surface))
  (gfx:height view))


(defmethod init-texture-device (view (device (eql :shader)) texture-device)
  (declare (ignorable device))
  (let* ((core-profile t)
	 (texture (gl:gen-texture))
	 (target :texture-rectangle)
	 (fixed-size (tex :size))
	 (width (if fixed-size (first (tex :size)) (width view)))
	 (height (if fixed-size (second (tex :size)) (height view)))
	 (renderer (make-instance 'renderer :width width :height height :core-profile core-profile))
	 (surface (make-instance 'shader-surface :width width :height height
				 :camera (if (tex :shared-camera) (camera (renderer *visual-canvas*))
					   (make-instance 'gfx:camera))
				 :renderer renderer
				 :shader (tex :src)
				 :texture-devices (tex :textures)
				 :gl-canvas (tex :gl-canvas)
				 :user-fn (tex :user-fn)))
	 (output (tex :output)))
    (resize-framebuffer renderer width height)
    (when output
      (setf (gethash output *io-surface-table*) (iosurface renderer)))
    (with-cgl-context ((cgl-context renderer))
      (let* ((fbo (if (tex :multisample) (fbo renderer)
		    (gfx::output-fbo (fbo renderer)))))
	(gfx:with-fbo (fbo)
	  (gfx:init surface))))
    (gl:bind-texture target texture)
    (cgl:tex-image-io-surface-2d (cgl-context view) target
				 :rgba width height :bgra
				 :unsigned-int-8-8-8-8-rev (iosurface renderer) 0)
    (gl:bind-texture target 0)
    (list device
	  :src (tex :src)
	  :tex-id texture
	  :target target
	  :surface surface
	  :multisample (tex :multisample)
	  :fixed-size fixed-size
	  :output output)))

(defmethod update-texture-device (view (device (eql :shader)) texture-device)
  (declare (ignore device))
  (let* ((width (width view))
  	 (height (height view))
  	 (surface (tex :surface))
	 (renderer (renderer surface))
	 (output (tex :output)))
    (when (and (not (tex :fixed-size))
	       (or (/= width (width renderer))
		   (/= height (height renderer))))
      (resize-framebuffer renderer width height)
      (when output
	(setf (gethash output *io-surface-table*) (iosurface renderer)))
      (setf (gfx:width surface) width (gfx:height surface) height)
      (gl:bind-texture (tex :target) (tex :tex-id))
      (cgl:tex-image-io-surface-2d (cgl-context view) (tex :target)
				   :rgba width height :bgra
				   :unsigned-int-8-8-8-8-rev (iosurface renderer) 0)
      (gl:bind-texture (tex :target) 0)
      (setf (resize-p surface) t))
    (gl:bind-texture (tex :target) (tex :tex-id))
    (with-cgl-context ((cgl-context renderer))
      (let* ((fbo (if (tex :multisample) (fbo renderer)
		    (gfx::output-fbo (fbo renderer)))))
	(gfx:with-fbo (fbo)
	  (gfx:draw surface))
	(gfx:with-fbo ((gfx::output-fbo (fbo renderer)))
	  (loop for unit in '(:texture0 :texture1 :texture2 :texture3
			      :texture4 :texture5 :texture6 :texture7)
		for device in (texture-devices surface)
		for target = (getf (cdr device) :target)
		do (gl:active-texture unit)
		   (case (car device)
		     (:previous-frame
		      (gl:copy-tex-image-2d target 0 :rgba8 0 0 (gfx:width surface) (gfx:height surface) 0)))
		   (gl:bind-texture target 0))))
      (gl:flush))))

(defmethod release-texture-device (view (device (eql :shader)) texture-device)
  (declare (ignore view device))
  (let* ((surface (tex :surface))
	 (renderer (renderer surface))
	 (output (tex :output)))
    (with-cgl-context ((cgl-context renderer))
      (let* ((fbo (if (tex :multisample) (fbo renderer)
		    (gfx::output-fbo (fbo renderer)))))
	(gfx:with-fbo (fbo)
	  (gfx:release surface))))
    (when output
      (remhash output *io-surface-table*))
    (release renderer)
    (gl:delete-texture (tex :tex-id))))




;;; ci-filter

(defmethod init-texture-device (view (device (eql :ci-filter)) texture-device)
  (declare (ignorable device))
  (let* ((core-profile nil)
	 (texture (gl:gen-texture))
	 (target :texture-rectangle)
	 (width (width view))
	 (height (height view))
	 (renderer (make-instance 'renderer :width width :height height :core-profile core-profile))
	 (surface (make-instance 'shader-surface :width width :height height
				 :renderer renderer :gl-canvas nil
				 :texture-devices nil))
	 (src (tex :src)))
    (resize-framebuffer renderer width height)
    (with-cgl-context ((cgl-context renderer))
      (let* ((fbo (if (tex :multisample) (fbo renderer)
		    (gfx::output-fbo (fbo renderer)))))
	(gfx:with-fbo (fbo)
	  (setf src (init-texture-device surface (car src) (cdr src))))))
    (gl:bind-texture target texture)
    (cgl:tex-image-io-surface-2d (cgl-context view) target
				 :rgba width height :bgra
				 :unsigned-int-8-8-8-8-rev (iosurface renderer) 0)
    (gl:bind-texture target 0)
    (list :ci-filter
	  :tex-id texture
	  :ci-context (ci:make-context (cgl-context renderer) (pixel-format renderer))
	  :output-filter (tex :output-filter)
	  :target target
	  :renderer renderer
	  :surface surface
	  :src src
	  :multisample (tex :multisample))))


(defmethod update-texture-device (view (device (eql :ci-filter)) texture-device)
  (let* ((width (width view))
	 (height (height view))
	 (src (tex :src))
	 (renderer (tex :renderer))
	 (surface (tex :surface)))
    (when (or (/= width (width renderer))
	      (/= height (height renderer)))
      (resize-framebuffer renderer width height)
      (setf (gfx:width surface) width (gfx:height surface) height)
      (gl:bind-texture (tex :target) (tex :tex-id))
      (cgl:tex-image-io-surface-2d (cgl-context view) (tex :target)
				   :rgba width height :bgra
				   :unsigned-int-8-8-8-8-rev (iosurface renderer) 0)
      (gl:bind-texture (tex :target) 0))
    (gl:bind-texture (tex :target) (tex :tex-id))
    (with-cgl-context ((cgl-context renderer))
      (let* ((fbo (if (tex :multisample) (fbo renderer)
		    (gfx::output-fbo (fbo renderer)))))
	(gfx:with-fbo (fbo)
	  (gl:active-texture :texture0)
	  (update-texture-device surface (car src) (cdr src))
	  (let* ((texture (getf (cdr src) :tex-id))
		 (fixed-size (getf (cdr src) :fixed-size))
		 (ci-image (ci:make-image-from-texture texture (apply #'ns:make-size (if fixed-size fixed-size (list width height)))))
		 (rect (ns:make-rect 0 0 width height)))
	    (dolist (filter (tex :output-filter))
	      (setf ci-image (ci:apply-filter filter ci-image)))
	    (gl:viewport 0 0 width height)
	    (gl:matrix-mode :projection)
	    (gl:load-identity)
	    (gl:ortho 0 width 0 height -100.0 100.0)
	    (gl:matrix-mode :modelview)
	    (gl:load-identity)
	    (ci:draw-image (tex :ci-context) ci-image rect 
			   (if fixed-size (ns:make-rect 0 0 (first fixed-size) (second fixed-size)) rect))
	    (gl:flush)))))))



(defmethod release-texture-device (view (device (eql :ci-filter)) texture-device)
  (declare (ignore view device))
  (let* ((renderer (tex :renderer))
	 (surface (tex :surface))
	 (src (tex :src)))
    (with-cgl-context ((cgl-context renderer))
      (let* ((fbo (if (tex :multisample) (fbo renderer)
		    (gfx::output-fbo (fbo renderer)))))
	(gfx:with-fbo (fbo)
	  (release-texture-device surface (car src) (cdr src))
	  (gfx:release surface))))
    (release renderer)
    (ns:release (tex :ci-context))
    (gl:delete-texture (tex :tex-id))))
