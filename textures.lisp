(in-package :cl-visual)

(defmacro tex (key)
  `(getf texture-device ,key))

(defmacro filter (default)
  (alexandria:with-gensyms (filter)
    `(let* ((,filter (getf texture-device :filter)))
       (if ,filter ,filter ,default))))

(defmacro wrap (default)
  (alexandria:with-gensyms (target wrap)
    `(let* ((,target (getf texture-device :target))
	    (,wrap (getf texture-device :wrap)))
       (cond ((eql ,target :texture-rectangle) :clamp-to-edge)
	     (,wrap ,wrap)
	     (t ,default)))))


;;; 
;;; audio-frame
;;; 
(defun initialize-audio-frame (frame-bus)
  (setf (getf (audio-data *visual-canvas*) :scope-synth)
    (sc:proxy :cl-visual-audio-frame
      (sc:with-controls ((bus frame-bus))
	(let* ((audio-data (audio-data *visual-canvas*))
	       (wavebuf (getf audio-data :wavebuf))
	       (freqbuf (getf audio-data :freqbuf))
	       (phase (- 1 (* 2 (sc:reciprocal 8192))))
	       (fft-buf (sc:local-buf 8192 1))
	       (n-samples (* 0.5 (- (sc:buf-samples.ir fft-buf) 2)))
	       (signal (sc:mix (sc:in.ar bus 2)))
	       (freqs (sc:fft fft-buf signal 0.5 1))
	       (indexer (+ n-samples 2
			   (* (sc:lf-saw.ar (/ 2 (sc:buf-dur.ir fft-buf)) phase)
			      n-samples)))
	       (indexer (sc::round~  indexer 2))
	       (s0 (sc:buf-rd.ar 1 fft-buf indexer 1 1))
	       (s1 (sc:buf-rd.ar 1 fft-buf (+ 1 indexer) 1 1))
	       (lin-mag (sqrt (+ (* s0 s0) (* s1 s1)))))
	  (declare (ignorable freqs))
	  (sc:record-buf.ar lin-mag freqbuf)
	  (sc:record-buf.ar (* 1.0 signal) wavebuf)
	  0.0))
      :to (audio-group *visual-canvas*)
      :fade .0)))

(defmethod init-texture-device (view (device (eql :audio-frame)) texture-device)
  (let* ((frame-bus (tex :frame-bus))
	 (synth (getf (audio-data *visual-canvas*) :scope-synth))
	 (texture (gl:gen-texture))
	 (filter (filter :linear))
	 (wrap (wrap :clamp-to-edge)))
    (gl:bind-texture  (tex :target) texture)
    (gl:tex-parameter (tex :target) :texture-mag-filter filter)
    (gl:tex-parameter (tex :target) :texture-min-filter filter)
    (gl:tex-parameter (tex :target) :texture-wrap-s wrap)
    (gl:tex-parameter (tex :target) :texture-wrap-t wrap)
    (gl:bind-texture  (tex :target) 0)
    (setf frame-bus (if frame-bus frame-bus 0))
    (if synth (sc::ctrl synth :bus frame-bus)
      (initialize-audio-frame frame-bus))    
    (list device
	  :context view :tex-id texture :target (tex :target))))

(defmethod update-texture-device (view (device (eql :audio-frame)) texture-device)
  (declare (ignorable view device))
  (let* ((audio-data (audio-data *visual-canvas*))
	 (wavebuf (sc:buffer-data (getf audio-data :wavebuf)))
	 (freqbuf (sc:buffer-data (getf audio-data :freqbuf)))
	 (scope-buffer (getf audio-data :scope-buffer)))
    (cffi:with-pointer-to-vector-data (wavebuf-ptr wavebuf)
      (cffi:with-pointer-to-vector-data (freqbuf-ptr freqbuf)
	(cffi:foreign-funcall "memcpy" :pointer scope-buffer :pointer freqbuf-ptr :sizet (* 4096 4))
	(cffi:foreign-funcall "memcpy" :pointer (cffi:inc-pointer scope-buffer (* 4096 4))
				       :pointer wavebuf-ptr
				       :sizet (* 4096 4))))
    (gl:bind-texture (tex :target) (tex :tex-id))
    (gl:tex-image-2d (tex :target) 0 :r32f 4096 2 0 :red :float scope-buffer)))

(defmethod release-texture-device (view (device (eql :audio-frame)) texture-device)
  (declare (ignorable view device texture-device))
  (let* ((synth (getf (audio-data *visual-canvas*) :scope-synth)))
    (sc:free synth)
    (setf (getf (audio-data *visual-canvas*) :scope-synth) nil)))


;; previous frame
(defmethod init-texture-device (view (device (eql :previous-frame)) texture-device)
  (declare (ignorable view texture-device))
  (let ((texture (gl:gen-texture))
	(filter (filter :linear))
	(wrap (wrap :clamp-to-edge)))
    (gl:bind-texture  (tex :target) texture)
    (gl:tex-parameter (tex :target) :texture-mag-filter filter)
    (gl:tex-parameter (tex :target) :texture-min-filter filter)
    (gl:tex-parameter (tex :target) :texture-wrap-s wrap)
    (gl:tex-parameter (tex :target) :texture-wrap-t wrap)
    (gl:bind-texture  (tex :target) 0)
    (list device :tex-id texture :target (tex :target))))

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
	 (format (ecase (cg:image-bits-per-pixel image)
		   (32 (list :rgba8 :rgba))
		   (24 (list :rgb8 :rgb))
		   (8 (list :red :red))))
	 (w (cg:image-width image))
	 (h (cg:image-height image))
	 (filter (filter :linear))
	 (wrap (wrap :repeat))
	 (mipmap (tex :mipmap))
	 (use-mipmap (and mipmap (eql (tex :target) :texture-2d))))
    (gl:bind-texture (tex :target) texture)
    (gl:tex-image-2d (tex :target) 0 (first format) w h 0 (second format) :unsigned-byte
		     (cg:image-bitmap-data image))
    (gl:tex-parameter (tex :target) :texture-mag-filter filter)
    (gl:tex-parameter (tex :target) :texture-min-filter (if use-mipmap :linear-mipmap-linear filter))
    (gl:tex-parameter (tex :target) :texture-wrap-s wrap)
    (gl:tex-parameter (tex :target) :texture-wrap-t wrap)
    (when use-mipmap
      (gl:generate-mipmap (tex :target)))
    (gl:bind-texture  (tex :target) 0)
    (list device :tex-id texture :target (tex :target))))


(defmethod update-texture-device (view (device (eql :image)) texture-device)
  (declare (ignorable view device))
  (gl:bind-texture (tex :target) (tex :tex-id)))

(defmethod release-texture-device (view (device (eql :image)) texture-device)
  (declare (ignorable view device))
  (gl:delete-texture (tex :tex-id)))



;;; 
;;; bitmap-context
;;;
(defmethod init-texture-device (view (device (eql :bitmap-context)) texture-device)
  (declare (ignorable view))
  (let* ((texture (gl:gen-texture))
	 (filter (filter :linear))
	 (wrap (wrap :repeat))
	 (context (tex :src)))
    (gl:bind-texture (tex :target) texture)
    (gl:tex-parameter (tex :target) :texture-mag-filter filter)
    (gl:tex-parameter (tex :target) :texture-min-filter filter)
    (gl:tex-parameter (tex :target) :texture-wrap-s wrap)
    (gl:tex-parameter (tex :target) :texture-wrap-t wrap)
    (gl:bind-texture  (tex :target) 0)
    (list device
	  :src context
	  :tex-id texture :target (tex :target))))

(defmethod update-texture-device (view (device (eql :bitmap-context)) texture-device)
  (declare (ignorable view device))
  (let* ((context (tex :src))
	 (w (cg:bitmap-width context))
	 (h (cg:bitmap-height context))
	 (data (cg:bitmap-data context)))
    (gl:bind-texture (tex :target) (tex :tex-id))
    (gl:tex-image-2d (tex :target) 0 :rgba8 w h 0 :rgba :unsigned-byte data)))

(defmethod release-texture-device (view (device (eql :bitmap-context)) texture-device)
  (declare (ignorable view device))
  (gl:delete-texture (tex :tex-id)))


;;;
;;; av-player
;;;
(defmethod init-texture-device (view (device av:player) texture-device)
  (declare (ignorable view texture-device))
  (let* ((texture-cache (core-video:make-texture-cache (cgl-context view)
						       (pixel-format view))))
    (list device
	  :texture-cache texture-cache
	  :target (tex :target))))

(defmethod update-texture-device (view (device av:player) texture-device)
  (declare (ignorable view))
  (av:with-texture-cache (device (tex :texture-cache) width height)))

(defmethod release-texture-device (view (device av:player) texture-device)
  (declare (ignorable view device))
  (core-video:release-texture-cache (tex :texture-cache)))




;;;
;;; screen frame legacy(CoreGraphics based)
;;; 
(defmethod init-texture-device (view (device (eql :cg-screen-frame)) texture-device)
  (let* ((rect (tex :src))
	 (texture (gl:gen-texture))
	 (filter (filter :linear))
	 (wrap (wrap :clamp-to-edge)))
    (gl:bind-texture  (tex :target) texture)
    (gl:tex-parameter (tex :target) :texture-mag-filter filter)
    (gl:tex-parameter (tex :target) :texture-min-filter filter)
    (gl:tex-parameter (tex :target) :texture-wrap-s wrap)
    (gl:tex-parameter (tex :target) :texture-wrap-t wrap)
    (gl:bind-texture  (tex :target) 0)
    (unless rect (setf rect (list 0 0 200 200)))
    (destructuring-bind (x y w h)
	rect
      (list device
	    :src (ns:make-rect x y w h)
	    :tex-id texture
	    :target (tex :target)))))

(defmethod update-texture-device (view (device (eql :cg-screen-frame)) texture-device)
  (declare (ignore view device))
  (let* ((rect (tex :src))
  	 (image (cg:image-from-screen rect)))
    (gl:bind-texture (tex :target) (tex :tex-id))
    (gl:tex-image-2d (tex :target) 0 :rgba8 (cg:image-width image) (cg:image-height image) 0
		     :rgba :unsigned-byte (cg:image-bitmap-data image))
    (cg:release-image image)))

(defmethod release-texture-device (view (device (eql :cg-screen-frame)) texture-device)
  (declare (ignore view device))
  (gl:delete-texture (tex :tex-id)))


;;; 
;;; av-capture
;;; 
(defmethod init-texture-device (view (device (eql :live-frame)) texture-device)
  (declare (ignorable view))
  (let* ((index (tex :src)))
    (unless index (setf index 0))
    (let* ((capture (av:make-camera-capture index))
	   (texture-cache (core-video:make-texture-cache (cgl-context view)
							 (pixel-format view))))
      (av:start-capture capture)
      (list capture
	    :release-p t
	    :texture-cache texture-cache
	    :target (tex :target)))))

(defmethod init-texture-device (view (device (eql :screen-frame)) texture-device)
  (declare (ignorable view))
  (let* ((rect (tex :src))
	 (fps (tex :fps)))
    (let* ((capture (av:make-screen-capture (when rect (apply #'ns:make-rect rect))
					    (if fps fps 60)))
	   (texture-cache (core-video:make-texture-cache (cgl-context view)
	   						 (pixel-format view))))
      (ns:queue-for-event-loop (lambda () (av:start-capture capture)))
      (list capture
	    :release-p t
	    :texture-cache texture-cache
	    :target (tex :target)))))


(defmethod init-texture-device (view (device av:capture) texture-device)
  (declare (ignorable view texture-device))
  (let* ((texture-cache (core-video:make-texture-cache (cgl-context view)
						       (pixel-format view))))
    (list device
	  :release-p nil
	  :texture-cache texture-cache
	  :target (tex :target))))

(defmethod update-texture-device (view (device av:capture) texture-device)
  (declare (ignorable view))
  (av:with-texture-cache (device (tex :texture-cache) width height)))

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
	    :target (tex :target)))))

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
					     #+ccl ccl::simple-short-float-vector)
				texture-device)
  (declare (ignorable view texture-device))
  (let* ((texture (gl:gen-texture))
	 (filter (filter :nearest))
	 (wrap (wrap :clamp-to-edge)))
    (gl:bind-texture  (tex :target) texture)
    (gl:tex-parameter (tex :target) :texture-min-filter filter)
    (gl:tex-parameter (tex :target) :texture-mag-filter filter)
    (gl:tex-parameter (tex :target) :texture-wrap-s wrap)
    (gl:tex-parameter (tex :target) :texture-wrap-t wrap)
    (gl:bind-texture  (tex :target) 0)
    (list device
	  :tex-id texture :target (tex :target))))

(defmethod update-texture-device (view (device #+sbcl sb-kernel::simple-array-single-float
					        #+ccl ccl::simple-short-float-vector)
				  texture-device)
  (declare (ignore view))
  (gl:bind-texture (tex :target) (tex :tex-id))
  (cffi:with-pointer-to-vector-data (ptr device)
    (gl:tex-image-2d (tex :target) 0 :r32f (length device) 1 0 :red :float ptr)))

(defmethod release-texture-device (view (device #+sbcl sb-kernel::simple-array-single-float
						#+ccl ccl::simple-short-float-vector)
				   texture-device)
  (declare (ignore view device))
  (gl:delete-texture (tex :tex-id)))

;;; Buffer of SuperCollider
(defmethod init-texture-device (view (device sc::buffer) texture-device)
  (declare (ignorable view texture-device))
  (let* ((texture (gl:gen-texture))
	 (filter (filter :nearest))
	 (wrap (wrap :clamp-to-edge)))
    (gl:bind-texture  (tex :target) texture)
    (gl:tex-parameter (tex :target) :texture-min-filter filter)
    (gl:tex-parameter (tex :target) :texture-mag-filter filter)
    (gl:tex-parameter (tex :target) :texture-wrap-s wrap)
    (gl:tex-parameter (tex :target) :texture-wrap-t wrap)
    (gl:bind-texture  (tex :target) 0)
    (list device
	  :tex-id texture :target (tex :target))))

(defmethod update-texture-device (view (device sc::buffer) texture-device)
  (declare (ignore view))
  (gl:bind-texture (tex :target) (tex :tex-id))
  (cffi:with-foreign-slots ((sc::snd-bufs) (sc::sc-world sc::*s*) (:struct sc::world))
    (let* ((data (getf (cffi:mem-aref sc::snd-bufs '(:struct sc::snd-buf)
				      (sc::bufnum device)) 'sc::data)))
      (gl:tex-image-2d (tex :target) 0 :r32f (min 44100 (* (sc:chanls device) (sc:frames device)))
		       1 0 :red :float data))))

(defmethod release-texture-device (view (device sc::buffer) texture-device)
  (declare (ignore view device))
  (gl:delete-texture (tex :tex-id)))

;; fbo
(defmethod init-texture-device (view (device (eql :fbo)) texture-device)
  (let* ((width (width view))
	 (height (height view))
	 (texture (gl:gen-texture))
	 (fbo nil)
	 (canvas (make-instance (tex :src) :width width :height height :camera (camera view)))
	 (filter (filter :linear))
	 (wrap (wrap :clamp-to-edge)))
    (unwind-protect (progn
		      (gl:bind-texture (tex :target) texture)
		      (gl:tex-image-2d (tex :target) 0 :rgba8 width height 0 :rgba
				       :unsigned-byte (cffi:null-pointer))
		      (gl:tex-parameter (tex :target) :texture-min-filter filter)
		      (gl:tex-parameter (tex :target) :texture-mag-filter filter)
		      (gl:tex-parameter (tex :target) :texture-wrap-s wrap)
		      (gl:tex-parameter (tex :target) :texture-wrap-t wrap)
		      (gl:bind-texture  (tex :target) 0)
		      (setf fbo (gfx:make-fbo width height :multisample t :texture texture :target (tex :target)))
		      (gfx:with-fbo (fbo) 
			(gfx::init canvas)))
      (gl:bind-framebuffer :framebuffer
			   (gfx::framebuffer (if (gl-canvas view) (fbo view) (gfx::output-fbo (fbo view))))))
    (list device
	  :tex-id texture :target (tex :target) :fbo fbo :gl-canvas canvas)))

(defmethod update-texture-device (view (device (eql :fbo)) texture-device)
  (declare (ignore device))
  (let* ((fbo (tex :fbo)))
    (unwind-protect
  	 (let* ((width (width view))
  		(height (height view)))
  	   (unless (and (= width (gfx:width fbo))
  			(= height (gfx:height fbo)))
  	     (gl:bind-texture (tex :target) (tex :tex-id))
  	     (gl:tex-image-2d (tex :target) 0 :rgba8 width height 0 :rgba
  			      :unsigned-byte (cffi:null-pointer))
  	     (gl:bind-texture (tex :target) 0)
  	     (gfx:reinit-fbo fbo width height)
  	     (setf (gfx:width (tex :gl-canvas)) width
  		   (gfx:height (tex :gl-canvas)) height))
	   (gl:bind-texture (tex :target) (tex :tex-id))
  	   (gfx:with-fbo (fbo)
  	     (gfx::draw (tex :gl-canvas))))
      (gl:bind-framebuffer :framebuffer
  			   (gfx::framebuffer (if (gl-canvas view) (fbo view) (gfx::output-fbo (fbo view))))))))

(defmethod release-texture-device (view (device (eql :fbo)) texture-device)
  (declare (ignorable view device))
  (unwind-protect (progn
  		      (gfx:with-fbo ((tex :fbo))
  			(gfx:release (tex :gl-canvas)))
  		      (gfx:release-environment (tex :gl-canvas))
  		      (gfx:release-fbo (tex :fbo))
  		      (gl:delete-texture (tex :tex-id)))
      (gl:bind-framebuffer :framebuffer
  			   (gfx::framebuffer (if (gl-canvas view) (fbo view) (gfx::output-fbo (fbo view)))))))



;;
(defvar *io-surface-table* (make-hash-table))


;;; gl-canvas
(defmethod init-texture-device (view (device (eql :gl-canvas)) texture-device)
  (declare (ignorable device))
  (let* ((core-profile (if (not (find :core-profile texture-device)) t
			 (tex :core-profile)))
	 (texture (gl:gen-texture))
	 (fixed-size (tex :size))
	 (width (if fixed-size (first (tex :size)) (width view)))
	 (height (if fixed-size (second (tex :size)) (height view)))
	 (renderer (make-instance 'renderer :width width :height height :core-profile core-profile))
	 (gl-canvas (make-instance (tex :src) :width width :height height
				   :camera (if (tex :shared-camera) (camera (renderer *visual-canvas*))
					     (make-instance 'gfx:camera))))
	 (output (tex :output)))
    (resize-framebuffer renderer width height)
    (when output
      (setf (gethash output *io-surface-table*) (iosurface renderer)))
    (with-cgl-context ((cgl-context renderer))
      (gfx:with-fbo ((fbo renderer))
	(gfx:init gl-canvas)))
    (gl:bind-texture (tex :target) texture)
    (cgl:tex-image-io-surface-2d (cgl-context view) (tex :target)
				 :rgba width height :bgra
				 :unsigned-int-8-8-8-8-rev (iosurface renderer) 0)
    (gl:bind-texture (tex :target) 0)
    (list device
	  :tex-id texture
	  :target (tex :target)
	  :renderer renderer
	  :gl-canvas gl-canvas
	  :fixed-size fixed-size
	  :output output)))

(defmethod update-texture-device (view (device (eql :gl-canvas)) texture-device)
  (declare (ignore device))
  (let* ((width (width view))
  	 (height (height view))
  	 (renderer (tex :renderer))
  	 (canvas (tex :gl-canvas))
	 (output (tex :output)))
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
      (gl:bind-texture (tex :target) 0))
    (gl:bind-texture (tex :target) (tex :tex-id))
    (with-cgl-context ((cgl-context renderer))
      (gfx:with-fbo ((fbo renderer))
	(setf (gfx:projection-matrix canvas) (kit.math:perspective-matrix 45.0 (/ width height) .1 10000.0)
	      (gfx:modelview-matrix canvas) (gfx:eval-camera (gfx:camera canvas)))
	(gfx:draw canvas))
      (gl:flush))))

(defmethod release-texture-device (view (device (eql :gl-canvas)) texture-device)
  (declare (ignore view device))
  (let* ((renderer (tex :renderer))
  	 (canvas (tex :gl-canvas))
	 (output (tex :output)))
    (with-cgl-context ((cgl-context renderer))
      (gfx:with-fbo ((fbo renderer))
	(gfx:release canvas))
      (gfx:release-environment canvas))
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
   (fn :initarg :fn :reader fn)))

(defmethod gfx:init ((view shader-surface))
  (let* ((devices (texture-devices view)))
    (let* ((pipeline (gethash (shader view) gfx::*all-pipeline-table*))
	   (targets (mapcar (lambda (type) (ecase type
					     (:sampler-2d :texture-2d)
					     (:sampler-2d-rect :texture-rectangle)))
			    (mapcar #'second (subseq (gfx::%pipeline-uniforms pipeline) 0 (length devices))))))
      (setf (texture-devices view)
	(loop for device in devices
	      for target in targets
	      collect (let ((device (alexandria:ensure-list device)))
			(init-texture-device view (car device) (append (cdr device)
								       (list :target target))))))
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
    (when (fn view)
      (funcall (fn view) view))
    (setf (gfx:projection-matrix view) (kit.math:perspective-matrix 45.0 (/ w h) .1 10000.0)
	  (gfx:modelview-matrix view) (gfx:eval-camera (gfx:camera view)))
    (loop for unit in '(:texture0 :texture1 :texture2 :texture3
			:texture4 :texture5 :texture6 :texture7)
	  for device in (texture-devices view)
	  do (gl:active-texture unit)
	     (update-texture-device view (car device) (cdr device)))
    (gl:enable :depth-test)
    (apply (shader view) view `(:triangles 0 6 ,(gpu-stream (renderer *visual-canvas*))
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
				:camera ,(list (gfx::eye-x (gfx:camera view))
					       (gfx::eye-y (gfx:camera view))
					       (gfx::eye-z (gfx:camera view)))
				:lookat ,(list (gfx::center-x (gfx:camera view))
					       (gfx::center-y (gfx:camera view))
					       (gfx::center-z (gfx:camera view)))
				:projection-matrix ,(gfx:projection-matrix view)
				:modelview-matrix ,(gfx:modelview-matrix view)))
    (when (gl-canvas view)
      (setf (gfx:width (gl-canvas view)) w (gfx:height (gl-canvas view)) h)
      (setf (gfx:projection-matrix (gl-canvas view)) (gfx:projection-matrix view)
	    (gfx:modelview-matrix (gl-canvas view)) (gfx:modelview-matrix view))
      (gfx:draw (gl-canvas view)))
    (gl:disable :depth-test)))

(defmethod gfx:release ((view shader-surface))
  (when (gl-canvas view)
    (gfx:release (gl-canvas view))
    (gfx:release-environment (gl-canvas view)))
  (loop for device in (texture-devices view)
	do (release-texture-device view (car device) (cdr device))))


(defmethod cgl-context ((view shader-surface))
  (cgl-context (renderer view)))

(defmethod width ((view shader-surface))
  (gfx:width view))

(defmethod height ((view shader-surface))
  (gfx:height view))


(defmethod init-texture-device (view (device (eql :shader)) texture-device)
  (declare (ignorable device))
  (let* ((core-profile (if (not (find :core-profile texture-device)) t
			 (tex :core-profile)))
	 (texture (gl:gen-texture))
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
				 :fn (tex :fn)))
	 (output (tex :output)))
    (resize-framebuffer renderer width height)
    (when output
      (setf (gethash output *io-surface-table*) (iosurface renderer)))
    (with-cgl-context ((cgl-context renderer))
      (gfx:with-fbo ((fbo renderer))
	(gfx:init surface)))
    (gl:bind-texture (tex :target) texture)
    (cgl:tex-image-io-surface-2d (cgl-context view) (tex :target)
				 :rgba width height :bgra
				 :unsigned-int-8-8-8-8-rev (iosurface renderer) 0)
    (gl:bind-texture (tex :target) 0)
    (list device
	  :src (tex :src)
	  :tex-id texture
	  :target (tex :target)
	  :surface surface
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
      (gl:bind-texture (tex :target) 0))
    (gl:bind-texture (tex :target) (tex :tex-id))
    (with-cgl-context ((cgl-context renderer))
      (gfx:with-fbo ((fbo renderer))
	(gfx:draw surface))
      (gl:flush))))

(defmethod release-texture-device (view (device (eql :shader)) texture-device)
  (declare (ignore view device))
  (let* ((surface (tex :surface))
	 (renderer (renderer surface))
	 (output (tex :output)))
    (with-cgl-context ((cgl-context renderer))
      (gfx:with-fbo ((fbo renderer))
	(gfx:release surface))
      (gfx:release-environment surface))
    (when output
      (remhash output *io-surface-table*))
    (release renderer)
    (gl:delete-texture (tex :tex-id))))

;;; io-surface
(defmethod init-texture-device (view (device (eql :io-surface)) texture-device)
  (let* ((src (tex :src))
	 (io-surface (gethash src *io-surface-table*)))
    (when io-surface
      (let* ((texture (gl:gen-texture))
	     (width (io-surface:width io-surface))
	     (height (io-surface:height io-surface)))
	(gl:bind-texture (tex :target) texture)
	(cgl:tex-image-io-surface-2d (cgl-context view) (tex :target)
				     :rgba width height :bgra
				     :unsigned-int-8-8-8-8-rev io-surface 0)
	(gl:bind-texture (tex :target) 0)
	(list device
	      :src (tex :src)
	      :tex-id texture
	      :target (tex :target)
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

