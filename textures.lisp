(in-package :cl-visual)

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

(defmethod process-texture-src (view (src (eql :audio-frame)) texture-src)
  (let* ((frame-bus (getf (cdr texture-src) :frame-bus))
	 (synth (getf (audio-data view) :scope-synth)))
    (setf frame-bus (if frame-bus frame-bus 0))
    (if synth (sc::ctrl synth :bus frame-bus)
      (initialize-audio-frame frame-bus))    
    (list :src src :context view :filter :linear :wrap :clamp-to-edge :flip-p nil
     :target :texture-2d)))

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
				       :sizet (* 4096 4) )))
    (gl:tex-image-2d :texture-2d 0 :r32f 4096 2 0 :red :float scope-buffer)))

(defmethod destroy-texture-src (view (src (eql :audio-frame)) texture-src new-texture-srcs)
  (declare (ignore view texture-src))
  (let* ((synth (getf (audio-data view) :scope-synth))
	 (find-audio-frame (loop for new-src in new-texture-srcs
				 do (when (eql :audio-frame (car (alexandria:ensure-list new-src)))
				      (return t)))))
    (unless find-audio-frame
      (sc:free synth)
      (setf (getf (audio-data view) :scope-synth) nil))))

;;;
;;; screen frame
;;; 
(defmethod process-texture-src (view (src (eql :screen-frame)) texture-src)
  (let* ((rect (getf (cdr texture-src) :rect)))
    (unless rect
      (setf rect (list 0 0 200 200)))
    (destructuring-bind (x y w h)
	rect
      (list :src src
       :filter (if-let ((filter (getf (cdr texture-src) :filter))) filter :linear)
       :wrap (if-let ((wrap (getf (cdr texture-src) :wrap))) wrap :clamp-to-edge)
       :rect (ns:make-rect x y w h)
       :target :texture-2d))))


(defmethod update-texture-src (view (src (eql :screen-frame)) texture-src)
  (declare (ignore view))
  (let* ((rect (getf texture-src :rect))
  	 (image (cg:image-from-screen rect)))
    (unwind-protect (gl:tex-image-2d :texture-2d 0 :rgba8 
				     (cg:image-width image) (cg:image-height image) 0
				     :rgba :unsigned-byte (cg:image-bitmap-data image))
      (cg:image-release image))))




;; previous frame
(defmethod process-texture-src (view (src (eql :previous-frame)) texture-src)
  (declare (ignore view texture-src))
  (list :src src :filter :linear :wrap :clamp-to-edge :flip-p nil :target :texture-2d))


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

(defmethod process-texture-src (view (src string) texture-src)
  (declare (ignore view))
  (destructuring-bind (filter wrap flip-p)
      (parse-texture-options (cdr texture-src))
    (list :src src :filter filter :wrap wrap :flip-p flip-p :target :texture-2d)))

(defmethod init-texture-src (view tex-id (src string) texture-src)
  (let* ((full-path (uiop:truenamize src)))
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
	(gl:bind-texture :texture-2d tex-id)
	(gl:tex-image-2d :texture-2d 0 (first format) w h 0 (second format) :unsigned-byte
			 (cg:image-bitmap-data image))
	(when (eql (getf texture-src :filter) :mipmap)
	  (gl:generate-mipmap :texture-2d))
	(default-gl-tex-parameter (getf texture-src :filter) (getf texture-src :wrap))
	(gl:bind-texture :texture-2d 0)))))


;;;
;;; av-player
;;;
(defmethod process-texture-src (view (src av:player) texture-src)
  (declare (ignore view texture-src))
  (list :src src :filter :linear :wrap :clamp-to-edge :flip-p nil :auto-release-p nil
   :target :texture-2d))

(defmethod update-texture-src (view (src av:player) texture-src)
  (declare (ignore view texture-src))
  (av:with-media-data (src width height data)
    (gl:tex-image-2d :texture-2d 0 :rgba8 width height 0 :rgba :unsigned-byte data)))

;;; 
;;; av-capture
;;; 
(defmethod process-texture-src (view (src (eql :live-frame)) texture-src)
  (declare (ignore view))
  (let* ((index (second texture-src)))
    (unless index (setf index 0))
    (let* ((capture (av:make-camera-capture index)))
      (av:start-capture capture)
      (list :src capture
	    :device-index index
	    :release-p t
	    :filter :linear :wrap :clamp-to-edge :flip-p nil
	    :target :texture-2d))))

(defmethod process-texture-src (view (src av:capture) texture-src)
  (declare (ignore view texture-src))
  (list :src src
	:release-p nil
	:filter :linear :wrap :clamp-to-edge :flip-p nil
	:target :texture-2d))

(defmethod update-texture-src (view (src av:capture) texture-src)
  (declare (ignore view texture-src))
  (av:with-media-data (src width height data)
    (gl:tex-image-2d :texture-2d 0 :rgba8 width height 0 :rgba :unsigned-byte data)))

(defmethod destroy-texture-src (view (src av:capture) texture-src new-texture-srcs)
  (declare (ignore view new-texture-srcs))
  (when (getf texture-src :release-p)
    (av:stop-capture src)
    (av:release-capture src)))

;;; 
;;; syphon
;;; 
(defmethod process-texture-src (view (src (eql :syphon)) texture-src)
  (if (find :syphon-client texture-src) (push :src texture-src)
    (let* ((app-key (second texture-src))
	   (name-key (third texture-src))
	   (syphon (syphon:get-server app-key (if name-key name-key ""))))
      (list :src :syphon :filter :linear :wrap :clamp-to-edge :flip-p nil
	    :app-key app-key
	    :name-key name-key
	    :size (list 0 0)
	    :syphon-client (when syphon
			     (syphon:make-client syphon (cgl-context view)))
	    :target :texture-rectangle))))

(defmethod init-texture-src (view tex-id (src (eql :syphon)) texture-src)
  (declare (ignore view tex-id src texture-src)))

(defmethod update-texture-src (view (src (eql :syphon)) texture-src)
  (alexandria:when-let ((syphon (getf texture-src :syphon-client)))
    (let* ((image (syphon:new-frame-image syphon)))
      (unless (cffi:null-pointer-p image)
	(ns:autorelease image)
	(let* ((size (syphon:texture-size image))
	       (w (ns:size-width size))
	       (h (ns:size-height size))
	       (orig-size (getf texture-src :size))
	       (name (syphon:texture-name image)))
	  (unless (and (= w (car orig-size))
	  		   (= h (second orig-size)))
	  	(format t "~&<Syphon-~s~@[|~s~]> image-size: ~a, ~a image-name: ~a~%"
	  		(getf texture-src :app-key)
	  		(getf texture-src :name-key)
	  		w h name)
	  	(setf (getf texture-src :size) (list w h)))
	  (gl:bind-texture :texture-rectangle name))))))

(defmethod destroy-texture-src (view (src (eql :syphon)) texture-src new-texture-srcs)
  (declare (ignore view new-texture-srcs))
  (alexandria:when-let ((syphon (getf texture-src :syphon-client)))
    (syphon:stop-client syphon)
    (ns:release syphon)))

;;; simple-array singloe-float
(defmethod process-texture-src (view (src simple-array) texture-src)
  (declare (ignore view texture-src))
  (list :src src :filter :nearest :wrap :clamp-to-edge :flip-p nil
	:target :texture-rectangle))

(defmethod init-texture-src (view tex-id (src simple-array) texture-src)
  (declare (ignore view texture-src))
  (gl:bind-texture :texture-rectangle tex-id)
  (gl:tex-parameter :texture-rectangle :texture-min-filter :nearest)
  (gl:tex-parameter :texture-rectangle :texture-mag-filter :nearest)
  (gl:tex-parameter :texture-rectangle :texture-wrap-s :clamp-to-edge)
  (gl:tex-parameter :texture-rectangle :texture-wrap-t :clamp-to-edge)
  (gl:bind-texture :texture-rectangle 0))

(defmethod update-texture-src (view (src simple-array) texture-src)
  (declare (ignore view texture-src))
  (cffi:with-pointer-to-vector-data (ptr src)
    (gl:tex-image-2d :texture-rectangle 0 :r32f (length src) 1 0 :red :float ptr)))

(defmethod destroy-texture-src (view (src simple-array) texture-src new-texture-srcs)
  (declare (ignore view texture-src new-texture-srcs)))


;;; Buffer of SuperCollider
(defmethod process-texture-src (view (src sc::buffer) texture-src)
  (declare (ignore view texture-src))
  (list :src src :filter :nearest :wrap :clamp-to-edge :flip-p nil
	:target :texture-rectangle))

(defmethod init-texture-src (view tex-id (src sc::buffer) texture-src)
  (declare (ignore view texture-src))
  (gl:bind-texture :texture-rectangle tex-id)
  (gl:tex-parameter :texture-rectangle :texture-min-filter :nearest)
  (gl:tex-parameter :texture-rectangle :texture-mag-filter :nearest)
  (gl:tex-parameter :texture-rectangle :texture-wrap-s :clamp-to-edge)
  (gl:tex-parameter :texture-rectangle :texture-wrap-t :clamp-to-edge)
  (gl:bind-texture :texture-rectangle 0))

(defmethod update-texture-src (view (src sc::buffer) texture-src)
  (declare (ignore view texture-src))
  (cffi:with-foreign-slots ((sc::snd-bufs) (sc::sc-world sc::*s*) (:struct sc::world))
    (let* ((data (getf (cffi:mem-aref sc::snd-bufs '(:struct sc::snd-buf)
				      (sc::bufnum src)) 'sc::data)))
      (gl:tex-image-2d :texture-rectangle 0 :r32f (min 44100 (* (sc:chanls src) (sc:frames src)))
		       1 0 :red :float data))))

(defmethod destroy-texture-src (view (src sc::buffer) texture-src new-texture-srcs)
  (declare (ignore view texture-src new-texture-srcs)))


;; fbo
(defmethod process-texture-src (view (src (eql :fbo)) texture-src)
  (let* ((width (width view))
	 (height (height view)))
    (list :src src :filter :linear :wrap :clamp-to-edge :flip-p nil 
	  :target :texture-2d
	  :gl-canvas (make-instance (second texture-src) :width width :height height :camera (camera view))
	  :fbo nil)))

(defmethod init-texture-src (view tex-id (src (eql :fbo)) texture-src)
  (unwind-protect (let* ((width (width view))
			   (height (height view)))
		      (gl:bind-texture :texture-2d tex-id)
		      (gl:tex-image-2d :texture-2d 0 :rgba8 width height 0 :rgba
				       :unsigned-byte (cffi:null-pointer))
		      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
		      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
		      (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
		      (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
		      (gl:bind-texture :texture-2d 0)
		      (setf (getf texture-src :fbo)
			(gfx:make-fbo width height :multisample t :texture tex-id))
		      (gfx:with-fbo ((getf texture-src :fbo))
			(gfx::init (getf texture-src :gl-canvas))))
      (gl:bind-framebuffer :framebuffer
			   (gfx::framebuffer (if (gl-canvas view) (fbo view) (gfx::output-fbo (fbo view)))))))

(defmethod update-texture-src (view (src (eql :fbo)) texture-src)
  (let* ((fbo (getf texture-src :fbo)))
    (unwind-protect
	 (let* ((width (width view))
		(height (height view)))
	   (unless (and (= width (gfx:width fbo))
			(= height (gfx:height fbo)))
	     (gl:bind-texture :texture-2d (gfx:output-texture fbo))
	     (gl:tex-image-2d :texture-2d 0 :rgba8 width height 0 :rgba
			      :unsigned-byte (cffi:null-pointer))
	     (gl:bind-texture :texture-2d 0)
	     (gfx:reinit-fbo fbo width height)
	     (setf (gfx:width (getf texture-src :gl-canvas)) width
		   (gfx:height (getf texture-src :gl-canvas)) height))
	   (gfx:with-fbo (fbo)
	     (gfx::draw (getf texture-src :gl-canvas))))
      (gl:bind-framebuffer :framebuffer
			   (gfx::framebuffer (if (gl-canvas view) (fbo view) (gfx::output-fbo (fbo view))))))))

(defmethod destroy-texture-src (view (src (eql :fbo)) texture-src new-texture-srcs)
  (declare (ignore new-texture-srcs))
  (unwind-protect (progn
		      (gfx:with-fbo ((getf texture-src :fbo))
			(gfx::shutdown (getf texture-src :gl-canvas)))
		      (gfx:cleanup-context (getf texture-src :gl-canvas))
		      (gfx:cleanup-fbo (getf texture-src :fbo)))
      (gl:bind-framebuffer :framebuffer
			   (gfx::framebuffer (if (gl-canvas view) (fbo view) (gfx::output-fbo (fbo view)))))))

;;; iosurface
(defmethod process-texture-src (view (src (eql :io-surface)) texture-src)
  (let* ((core-profile (if (not (find :core-profile texture-src)) t
			 (getf texture-src :core-profile)))
	 (fixed-size (getf texture-src :size))
	 (width (if fixed-size (first (getf texture-src :size)) (width view)))
	 (height (if fixed-size (second (getf texture-src :size)) (height view)))
	 (use-mouse (getf texture-src :use-mouse)))
    (list :src src :filter :linear :wrap :clamp-to-edge :flip-p nil 
	  :target :texture-rectangle
	  :draw-fbo nil
	  :renderer (make-instance 'renderer :width width :height height :core-profile core-profile)
	  :gl-canvas (make-instance (second texture-src) :width width :height height
				    :camera (when use-mouse (camera view)))
	  :io-surface nil
	  :fixed-size fixed-size)))

(defmethod init-texture-src (view tex-id (src (eql :io-surface)) texture-src)
  (let* ((width (width view))
	 (height (height view))
	 (renderer (getf texture-src :renderer))
	 (canvas (getf texture-src :gl-canvas)))
    (resize-framebuffer renderer width height)
    (with-cgl-context ((cgl-context renderer))
      (gfx:with-fbo ((fbo renderer))
	(gfx:init canvas)))
    (setf (getf texture-src :io-surface) (io-surface:lookup (io-surface:id (iosurface renderer))))
    (gl:bind-texture :texture-rectangle tex-id)
    (cgl:tex-image-io-surface-2d (cgl-context view) :texture-rectangle :rgba width height
				 :bgra :unsigned-int-8-8-8-8-rev (getf texture-src :io-surface) 0)
    (gl:bind-texture :texture-rectangle 0)))


(defmethod update-texture-src (view (src (eql :io-surface)) texture-src)
  (let* ((width (width view))
  	 (height (height view))
  	 (renderer (getf texture-src :renderer))
  	 (canvas (getf texture-src :gl-canvas)))
    (when (and (not (getf texture-src :fixed-size))
	       (or (/= width (width renderer))
		   (/= height (height renderer))))
      (setf (width renderer) width (height renderer) height)
      (setf (gfx:width canvas) width (gfx:height canvas) height)
      (ns:release (getf texture-src :io-surface))
      (let* ((io-surface (io-surface:lookup (io-surface:id (iosurface renderer)))))
	(setf (getf texture-src :io-surface) io-surface)
	(cgl:tex-image-io-surface-2d (cgl-context view) :texture-rectangle
				     :rgba width height :bgra
				     :unsigned-int-8-8-8-8-rev (getf texture-src :io-surface) 0))
      (resize-framebuffer renderer width height))
    (with-cgl-context ((cgl-context renderer))
      (gfx:with-fbo ((fbo renderer))
	(gfx:draw canvas))
      (gl:flush))))

(defmethod destroy-texture-src (view (src (eql :io-surface)) texture-src new-texture-srcs)
  (declare (ignore view new-texture-srcs))
  (let* ((renderer (getf texture-src :renderer))
  	 (canvas (getf texture-src :gl-canvas)))
    (with-cgl-context ((cgl-context renderer))
      (gfx:with-fbo ((fbo renderer))
	(gfx:shutdown canvas))
      (gfx:cleanup-context canvas))
    (destroy renderer)
    (ns:release (getf texture-src :io-surface))))

