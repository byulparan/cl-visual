(in-package :cl-visual)

(defmacro tex (key)
  `(getf texture-src ,key))

;;; 
;;; audio-frame
;;; 
(defun initialize-audio-frame (frame-bus)
  (setf (getf (audio-data *visual-canvas*) :scope-synth)
    (sc:proxy :shadertoy-audio-frame
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

(defmethod init-texture-src (view (src (eql :audio-frame)) texture-src)
  (let* ((frame-bus (tex :frame-bus))
	 (synth (getf (audio-data view) :scope-synth))
	 (texture (gl:gen-texture)))
    (setf frame-bus (if frame-bus frame-bus 0))
    (if synth (sc::ctrl synth :bus frame-bus)
      (initialize-audio-frame frame-bus))    
    (list :src src :context view :filter :linear :wrap :clamp-to-edge :flip-p nil
	  :tex-id texture :target :texture-2d)))

(defmethod update-texture-src (view (src (eql :audio-frame)) texture-src)
  (declare (ignore view texture-src))
  (let* ((audio-data (audio-data view))
	 (wavebuf (sc:buffer-data (getf audio-data :wavebuf)))
	 (freqbuf (sc:buffer-data (getf audio-data :freqbuf)))
	 (scope-buffer (getf audio-data :scope-buffer)))
    (cffi:with-pointer-to-vector-data (wavebuf-ptr wavebuf)
      (cffi:with-pointer-to-vector-data (freqbuf-ptr freqbuf)
	(cffi:foreign-funcall "memcpy" :pointer scope-buffer :pointer freqbuf-ptr :sizet (* 4096 4))
	(cffi:foreign-funcall "memcpy" :pointer (cffi:inc-pointer scope-buffer (* 4096 4))
				       :pointer wavebuf-ptr
				       :sizet (* 4096 4))))
    (gl:bind-texture :texture-2d (tex :tex-id))
    (gl:tex-image-2d :texture-2d 0 :r32f 4096 2 0 :red :float scope-buffer)))

(defmethod destroy-texture-src (view (src (eql :audio-frame)) texture-src)
  (declare (ignore view texture-src))
  (let* ((synth (getf (audio-data view) :scope-synth)))
    (sc:free synth)
    (setf (getf (audio-data view) :scope-synth) nil)))

;;;
;;; screen frame
;;; 
(defmethod init-texture-src (view (src (eql :screen-frame)) texture-src)
  (let* ((rect (tex :rect))
	 (texture (gl:gen-texture)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-wrap-s (tex :wrap))
    (gl:tex-parameter :texture-2d :texture-wrap-t (tex :wrap))
    (gl:bind-texture :texture-2d 0)
    (unless rect (setf rect (list 0 0 200 200)))
    (destructuring-bind (x y w h)
	rect
      (list :src src
	    :filter (if-let ((filter (tex :filter))) filter :linear)
	    :wrap (if-let ((wrap (tex :wrap))) wrap :repeat)
	    :rect (ns:make-rect x y w h)
	    :tex-id texture
	    :target :texture-2d))))

(defmethod update-texture-src (view (src (eql :screen-frame)) texture-src)
  (declare (ignore view))
  (let* ((rect (tex :rect))
  	 (image (cg:image-from-screen rect)))
    (gl:bind-texture (tex :target) (tex :tex-id))
    (gl:tex-image-2d :texture-2d 0 :rgba8 (cg:image-width image) (cg:image-height image) 0
		     :rgba :unsigned-byte (cg:image-bitmap-data image))
    (cg:image-release image)))

(defmethod destroy-texture-src (view (src (eql :screen-frame)) texture-src)
  (declare (ignore view src))
  (gl:delete-texture (tex :tex-id)))


;; previous frame
(defmethod init-texture-src (view (src (eql :previous-frame)) texture-src)
  (declare (ignore view texture-src))
  (let ((texture (gl:gen-texture))
	(filter :linear)
	(wrap :clamp-to-edge))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-mag-filter filter)
    (gl:tex-parameter :texture-2d :texture-min-filter filter)
    (gl:tex-parameter :texture-2d :texture-wrap-s wrap)
    (gl:tex-parameter :texture-2d :texture-wrap-t wrap)
    (gl:bind-texture :texture-2d 0)
    (list :src src :filter filter :wrap wrap :flip-p nil
	  :tex-id texture :target :texture-2d)))

(defmethod update-texture-src (view (src (eql :previous-frame)) texture-src)
  (declare (ignore view src))
  (gl:bind-texture :texture-2d (tex :tex-id)))

(defmethod destroy-texture-src (view (src (eql :previous-frame)) texture-src)
  (declare (ignore view src))
  (gl:delete-texture (tex :tex-id)))


;;; 
;;; image
;;;
(defun parse-texture-options (options)
  (let* ((filter (getf options :filter))
	 (wrap (getf options :wrap))
	 (flip-p (or (not (find :flip-p options))
		     (getf options :flip-p))))
    (list (if filter filter :mipmap)
	  (if wrap wrap :repeat)
	  flip-p)))

(defmethod init-texture-src (view (src string) texture-src)
  (declare (ignore view))
  (destructuring-bind (filter wrap flip-p)
      (parse-texture-options texture-src)
    (let* ((texture (gl:gen-texture))
	   (full-path (uiop:truenamize src)))
      (assert full-path nil "~s : can't find image file" src)
      (setf full-path (namestring full-path))
      (let* ((image (gethash full-path (tex-image-table view))))
	(unless image
	  (setf image (cg:load-image full-path)
		(gethash full-path (tex-image-table view)) image))
	(let* ((format (ecase (cg:image-bits-per-pixel image)
			 (32 (list :rgba8 :rgba))
			 (24 (list :rgb8 :rgb))
			 (8 (list :red :red))))
	       (w (cg:image-width image))
	       (h (cg:image-height image)))
	  (gl:bind-texture :texture-2d texture)
	  (gl:tex-image-2d :texture-2d 0 (first format) w h 0 (second format) :unsigned-byte
			   (cg:image-bitmap-data image))
	  (when (eql filter :mipmap)
	    (gl:generate-mipmap :texture-2d))
	  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
	  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
	  (gl:tex-parameter :texture-2d :texture-wrap-s wrap)
	  (gl:tex-parameter :texture-2d :texture-wrap-t wrap)
	  (gl:bind-texture :texture-2d 0)))
      (list :src src :filter filter :wrap wrap :flip-p flip-p
	    :tex-id texture
	    :target :texture-2d))))

(defmethod update-texture-src (view (src string) texture-src)
  (gl:bind-texture :texture-2d (tex :tex-id)))

(defmethod destroy-texture-src (view (src string) texture-src)
  (gl:delete-texture (tex :tex-id)))

;;;
;;; av-player
;;;
(defmethod init-texture-src (view (src av:player) texture-src)
  (declare (ignore view texture-src))
  (let* ((texture (gl:gen-texture)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-mag-filter (tex :filter))
    (gl:tex-parameter :texture-2d :texture-min-filter (tex :filter))
    (gl:tex-parameter :texture-2d :texture-wrap-s (tex :wrap))
    (gl:tex-parameter :texture-2d :texture-wrap-t (tex :wrap))
    (gl:bind-texture :texture-2d 0)
    (list :src src :filter :linear :wrap :clamp-to-edge :flip-p nil :auto-release-p nil
	  :tex-id texture
	  :target :texture-2d)))

(defmethod update-texture-src (view (src av:player) texture-src)
  (declare (ignore view texture-src))
  (gl:bind-texture (tex :target) (tex :tex-id))
  (av:with-media-data (src width height data)
    (gl:tex-image-2d :texture-2d 0 :rgba8 width height 0 :rgba :unsigned-byte data)))

(defmethod destroy-texture-src (view (src av:player) texture-src)
  (declare (ignore view src))
  (gl:delete-texture (tex :tex-id)))

;;; 
;;; av-capture
;;; 
(defmethod init-texture-src (view (src (eql :live-frame)) texture-src)
  (declare (ignore view))
  (let* ((index (tex :index)))
    (unless index (setf index 0))
    (let* ((capture (av:make-camera-capture index))
	   (texture (gl:gen-texture))
	   (filter :linear)
	   (wrap :clamp-to-edge))
      (gl:bind-texture :texture-2d texture)
      (gl:tex-parameter :texture-2d :texture-mag-filter filter)
      (gl:tex-parameter :texture-2d :texture-min-filter filter)
      (gl:tex-parameter :texture-2d :texture-wrap-s wrap)
      (gl:tex-parameter :texture-2d :texture-wrap-t wrap)
      (gl:bind-texture :texture-2d 0)
      (av:start-capture capture)
      (list :src capture
	    :device-index index
	    :release-p t
	    :filter filter  :wrap wrap :flip-p nil
	    :tex-id texture
	    :target :texture-2d))))

(defmethod init-texture-src (view (src av:capture) texture-src)
  (declare (ignore view texture-src))
  (let* ((texture (gl:gen-texture))
	 (filter :linear)
	 (wrap :clamp-to-edge))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-mag-filter filter)
    (gl:tex-parameter :texture-2d :texture-min-filter filter)
    (gl:tex-parameter :texture-2d :texture-wrap-s wrap)
    (gl:tex-parameter :texture-2d :texture-wrap-t wrap)
    (gl:bind-texture :texture-2d 0)
    (list :src src
	  :release-p nil
	  :filter filter :wrap wrap :flip-p nil
	  :tex-id texture
	  :target :texture-2d)))

(defmethod update-texture-src (view (src av:capture) texture-src)
  (declare (ignore view src))
  (gl:bind-texture (tex :target) (tex :tex-id))
  (av:with-media-data (src width height data)
    (gl:tex-image-2d :texture-2d 0 :rgba8 width height 0 :rgba :unsigned-byte data)))

(defmethod destroy-texture-src (view (src av:capture) texture-src)
  (declare (ignore view))
  (when (getf texture-src :release-p)
    (av:stop-capture src)
    (av:release-capture src))
  (gl:delete-texture (tex :tex-id)))

;;; 
;;; syphon
;;; 
(defmethod init-texture-src (view (src (eql :syphon)) texture-src)
  (list :src :syphon :filter :linear :wrap :clamp-to-edge :flip-p nil
	:app (tex :app)
	:name (tex :name)
	:size (list 0 0)
	:syphon-client nil
	:target :texture-rectangle))

(defmethod update-texture-src (view (src (eql :syphon)) texture-src)
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
	    (format t "~&<Syphon-~s~@[|~s~]> image-size: ~a, ~a image-name: ~a~%"
		    (tex :app-key) (tex :name-key) w h name)
	    (setf (tex :size) (list w h)))
	  (gl:bind-texture :texture-rectangle name))))))

(defmethod destroy-texture-src (view (src (eql :syphon)) texture-src)
  (declare (ignore view src))
  (when-let ((syphon (tex :syphon-client)))
    (syphon:stop-client syphon)
    (ns:release syphon)))

;;; simple-array singloe-float
(defmethod init-texture-src (view (src sb-kernel::simple-array-single-float) texture-src)
  (declare (ignore view texture-src))
  (let* ((texture (gl:gen-texture)))
    (gl:bind-texture :texture-rectangle texture)
    (gl:tex-parameter :texture-rectangle :texture-min-filter :nearest)
    (gl:tex-parameter :texture-rectangle :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-rectangle :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-rectangle :texture-wrap-t :clamp-to-edge)
    (gl:bind-texture :texture-rectangle 0)
    (list :src src :filter :nearest :wrap :clamp-to-edge :flip-p nil
	  :tex-id texture :target :texture-rectangle)))

(defmethod update-texture-src (view (src sb-kernel::simple-array-single-float) texture-src)
  (declare (ignore view))
  (gl:bind-texture :texture-rectangle (tex :tex-id))
  (cffi:with-pointer-to-vector-data (ptr src)
    (gl:tex-image-2d :texture-rectangle 0 :r32f (length src) 1 0 :red :float ptr)))

(defmethod destroy-texture-src (view (src sb-kernel::simple-array-single-float) texture-src)
  (declare (ignore view src))
  (gl:delete-texture (tex :tex-id)))

;;; Buffer of SuperCollider
(defmethod init-texture-src (view (src sc::buffer) texture-src)
  (declare (ignore view texture-src))
  (let* ((texture (gl:gen-texture)))
    (gl:bind-texture :texture-rectangle texture)
    (gl:tex-parameter :texture-rectangle :texture-min-filter :nearest)
    (gl:tex-parameter :texture-rectangle :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-rectangle :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-rectangle :texture-wrap-t :clamp-to-edge)
    (gl:bind-texture :texture-rectangle 0)
    (list :src src :filter :nearest :wrap :clamp-to-edge :flip-p nil
	  :tex-id texture :target :texture-rectangle)))

(defmethod update-texture-src (view (src sc::buffer) texture-src)
  (declare (ignore view))
  (gl:bind-texture :texture-rectangle (tex :tex-id))
  (cffi:with-foreign-slots ((sc::snd-bufs) (sc::sc-world sc::*s*) (:struct sc::world))
    (let* ((data (getf (cffi:mem-aref sc::snd-bufs '(:struct sc::snd-buf)
				      (sc::bufnum src)) 'sc::data)))
      (gl:tex-image-2d :texture-rectangle 0 :r32f (min 44100 (* (sc:chanls src) (sc:frames src)))
		       1 0 :red :float data))))

(defmethod destroy-texture-src (view (src sc::buffer) texture-src)
  (declare (ignore view src))
  (gl:delete-texture (tex :tex-id)))

;; fbo
(defmethod init-texture-src (view (src (eql :fbo)) texture-src)
  (let* ((width (width view))
	 (height (height view))
	 (texture (gl:gen-texture))
	 (fbo nil)
	 (canvas (make-instance (tex :class) :width width :height height :camera (camera view))))
    (unwind-protect (progn
		      (gl:bind-texture :texture-2d texture)
		      (gl:tex-image-2d :texture-2d 0 :rgba8 width height 0 :rgba
				       :unsigned-byte (cffi:null-pointer))
		      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
		      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
		      (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
		      (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
		      (gl:bind-texture :texture-2d 0)
		      (setf fbo (gfx:make-fbo width height :multisample t :texture texture))
		      (gfx:with-fbo (fbo) 
			(gfx::init canvas)))
      (gl:bind-framebuffer :framebuffer
			   (gfx::framebuffer (if (gl-canvas view) (fbo view) (gfx::output-fbo (fbo view))))))
    (list :src src :filter :linear :wrap :clamp-to-edge :flip-p nil 
	  :tex-id texture :target :texture-2d :fbo fbo :gl-canvas canvas)))

(defmethod update-texture-src (view (src (eql :fbo)) texture-src)
  (let* ((fbo (tex :fbo)))
    (unwind-protect
  	 (let* ((width (width view))
  		(height (height view)))
  	   (unless (and (= width (gfx:width fbo))
  			(= height (gfx:height fbo)))
  	     (gl:bind-texture :texture-2d (tex :tex-id))
  	     (gl:tex-image-2d :texture-2d 0 :rgba8 width height 0 :rgba
  			      :unsigned-byte (cffi:null-pointer))
  	     (gl:bind-texture :texture-2d 0)
  	     (gfx:reinit-fbo fbo width height)
  	     (setf (gfx:width (tex :gl-canvas)) width
  		   (gfx:height (tex :gl-canvas)) height))
	   (gl:bind-texture :texture-2d (tex :tex-id))
  	   (gfx:with-fbo (fbo)
  	     (gfx::draw (tex :gl-canvas))))
      (gl:bind-framebuffer :framebuffer
  			   (gfx::framebuffer (if (gl-canvas view) (fbo view) (gfx::output-fbo (fbo view))))))))

(defmethod destroy-texture-src (view (src (eql :fbo)) texture-src)
  (declare (ignore view src))
  (unwind-protect (progn
  		      (gfx:with-fbo ((tex :fbo))
  			(gfx::shutdown (tex :gl-canvas)))
  		      (gfx:cleanup-context (getf texture-src :gl-canvas))
  		      (gfx:cleanup-fbo (getf texture-src :fbo))
  		      (gl:delete-texture (tex :tex-id)))
      (gl:bind-framebuffer :framebuffer
  			   (gfx::framebuffer (if (gl-canvas view) (fbo view) (gfx::output-fbo (fbo view)))))))


;;; iosurface
(defmethod init-texture-src (view (src (eql :io-surface)) texture-src)
  (declare (ignore src))
  (let* ((core-profile (if (not (find :core-profile texture-src)) t
			 (tex :core-profile)))
	 (texture (gl:gen-texture))
	 (fixed-size (tex :size))
	 (width (if fixed-size (first (tex :size)) (width view)))
	 (height (if fixed-size (second (tex :size)) (height view)))
	 (renderer (make-instance 'renderer :width width :height height :core-profile core-profile))
	 (use-mouse (tex :use-mouse))
	 (gl-canvas (make-instance (tex :class) :width width :height height
				   :camera (when use-mouse (camera view)))))
    (resize-framebuffer renderer width height)
    (let* ((io-surface (io-surface:lookup (io-surface:id (iosurface renderer)))))
      (with-cgl-context ((cgl-context renderer))
	(gfx:with-fbo ((fbo renderer))
	  (gfx:init gl-canvas)))
      (gl:bind-texture :texture-rectangle texture)
      (cgl:tex-image-io-surface-2d (cgl-context view) :texture-rectangle
				   :rgba width height :bgra
				   :unsigned-int-8-8-8-8-rev io-surface 0)
      (gl:bind-texture :texture-rectangle 0)
      (list :src src :filter :linear :wrap :clamp-to-edge :flip-p nil
	    :tex-id texture
	    :target :texture-rectangle
	    :renderer renderer
	    :gl-canvas gl-canvas
	    :io-surface io-surface
	    :fixed-size fixed-size))))

(defmethod update-texture-src (view (src (eql :io-surface)) texture-src)
  (declare (ignore src))
  (let* ((width (width view))
  	 (height (height view))
  	 (renderer (tex :renderer))
  	 (canvas (tex :gl-canvas)))
    (when (and (not (tex :fixed-size))
	       (or (/= width (width renderer))
		   (/= height (height renderer))))
      (resize-framebuffer renderer width height)
      (setf (gfx:width canvas) width (gfx:height canvas) height)
      (ns:release (tex :io-surface))
      (let* ((io-surface (io-surface:lookup (io-surface:id (iosurface renderer)))))
	(setf (tex :io-surface) io-surface)
	(gl:bind-texture :texture-rectangle (tex :tex-id))
	(cgl:tex-image-io-surface-2d (cgl-context view) :texture-rectangle
				     :rgba width height :bgra
				     :unsigned-int-8-8-8-8-rev (getf texture-src :io-surface) 0)
	(gl:bind-texture :texture-rectangle 0)))
    (gl:bind-texture :texture-rectangle (tex :tex-id))
    (with-cgl-context ((cgl-context renderer))
      (gfx:with-fbo ((fbo renderer))
	(gfx:draw canvas))
      (gl:flush))))

(defmethod destroy-texture-src (view (src (eql :io-surface)) texture-src)
  (declare (ignore view src))
  (let* ((renderer (tex :renderer))
  	 (canvas (tex :gl-canvas)))
    (with-cgl-context ((cgl-context renderer))
      (gfx:with-fbo ((fbo renderer))
	(gfx:shutdown canvas))
      (gfx:cleanup-context canvas))
    (destroy renderer)
    (ns:release (tex :io-surface))))

