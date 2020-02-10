(in-package :cl-visual)

(defun gfx::clear-pipeline ()
  (gfx:reinit-shader-system)
  (loop for pipeline being the hash-values of gfx::*all-pipeline-table*
 	do (setf (gfx::%pipeline-used-funcs pipeline) nil)))

(gfx:defpipeline (draw-framebuffer :version 120) ((image :sampler-2d-rect)
						  (image-resolution :float)
						  (gfx::iresolution :vec2))
  (:vertex ((pos :vec2) (coord :vec2))
	   (values (v! pos .0 1.0)
		   coord))
  (:fragment ((c :vec2))
	     (texture image (* c gfx::iresolution image-resolution))))

(defclass visual-canvas (ns:opengl-view gfx:gl-context)
  ((mailbox
    :initarg :mailbox
    :initform (make-mailbox)
    :reader mailbox)
   (frame-stream :initform
		 (gfx:make-gpu-stream '((pos :vec2) (coord :vec2))
				      '(-1.0 -1.0  0.0 0.0 1.0 -1.0  1.0 0.0
					-1.0  1.0  0.0 1.0 -1.0  1.0  0.0 1.0
					1.0 -1.0  1.0 0.0 1.0  1.0  1.0 1.0)
				      :core-profile nil)
		 :reader frame-stream)
   (view-size
    :initform nil
    :accessor view-size)
   (scene-ratio
    :initarg :scene-ratio
    :accessor scene-ratio)
   (renderer
    :initarg :renderer
    :reader renderer)
   (iosurface
    :initform nil
    :accessor iosurface)
   (framebuffer
    :initform nil
    :accessor framebuffer)
   (depthbuffer
    :initform nil
    :accessor depthbuffer)
   (texture
    :initform nil
    :accessor texture)
   (ci-context
    :accessor ci-context)
   (user-fn
    :initform nil
    :accessor user-fn)
   (output-filter
    :accessor output-filter)
   (audio-group
    :accessor audio-group)
   (audio-data
    :initform  nil
    :accessor audio-data
    :allocation :class)
   (fps-info
    :accessor fps-info)
   (info
    :initarg :info
    :accessor info)
   (syphon
    :initform nil
    :accessor syphon)
   (window
    :accessor window)
   (retina
    :initarg :retina
    :initform nil
    :accessor retina)
   (last-draw-time
    :initform 0
    :accessor last-draw-time)))



(defun make-framebuffer-from-iosurface (cgl-context framebuffer texture renderbuffer iosurface width height)
  (gl:bind-texture :texture-rectangle texture)
  (cgl:tex-image-io-surface-2d cgl-context :texture-rectangle :rgba width height :bgra :unsigned-int-8-8-8-8-rev iosurface 0)
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


(defmethod resize-framebuffer ((renderer visual-canvas) width height)
  (let* ((cgl-context (ns:cgl-context renderer)))
    (when (iosurface renderer) (ns:release (iosurface renderer)))
    (setf (iosurface renderer) (io-surface:lookup (io-surface:id (iosurface (renderer renderer)))))
    (unless (framebuffer renderer)
      (setf (framebuffer renderer) (gl:gen-framebuffer)
	    (texture renderer) (gl:gen-texture)
	    (depthbuffer renderer) (gl:gen-renderbuffer)))
    (make-framebuffer-from-iosurface cgl-context (framebuffer renderer) (texture renderer) (depthbuffer renderer)
				       (iosurface renderer) width height)))


(defmethod ns:init ((view visual-canvas))
  (setf (fps-info view) (make-fps-info))
  (update-visual-canvas view)
  (let* ((w (width (renderer view)))
	 (h (height (renderer view))))
    (resize-framebuffer view w h))
  (setf (ci-context view) (ci:make-context (ns:cgl-context view) (ns:cgl-pixel-format view)))
  (unless (audio-data view)
    (setf (audio-data view) (list :wavebuf (sc:buffer-alloc 4096)
				  :freqbuf (sc:buffer-alloc 4096)
				  :scope-buffer (cffi:foreign-alloc :float :count (* 4096 2))
				  :scope-synth nil)))
  (let* ((group (sc:make-group :pos :tail :to 0)))
    (sc:proxy :cl-visual-volumes
      (progn
  	(sc:out.kr *ivolume-index* (sc:a2k.kr (sc:lag.ar (abs (sc:mix (sc:in.ar 0 2))) .1)))
  	(dotimes (i (1- *num-ivolume*))
  	  (sc:out.kr (+ *ivolume-index* (+ 1 i)) (sc:a2k.kr (sc:lag.ar (abs (sc:mix (sc:in.ar (+ 82 (* i 2)) 2))) .1))))
  	0.0)
      :to group)
    (setf (audio-group view) group)))


(defun convert-size-to-backing (visual-canvas)
  (let* ((best-size (ns:objc visual-canvas "convertSizeToBacking:"
			     (:struct ns:size) (ns:make-size (ns:width visual-canvas)
							     (ns:height visual-canvas))
			     (:struct ns:size))))
    (list (ns:size-width best-size) (ns:size-height best-size))))

(defmethod ns:reshape ((view visual-canvas))
  (gl:clear-color .0 .0 .0 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (let* ((old-size (view-size view))
	 (new-size (if (retina view) (convert-size-to-backing view)
		     (list (ns:width view) (ns:height view)))))
    (when (or (/= (car new-size) (car old-size))
	      (/= (second new-size) (second old-size)))
      (send-message (mailbox view) :force-resize))))

(defun update-visual-canvas (canvas)
  (unless (mailbox-empty-p (mailbox canvas))
    (let* ((options (receive-message (mailbox canvas)))
	   (scene-size nil))
      (flet ((get-scene-size (scene-ratio best-size)
	       (list (floor (* scene-ratio (first best-size)))
		     (floor (* scene-ratio (second best-size))))))
	(if (eql options :force-resize)
	    (let ((scene-ratio (scene-ratio canvas))
		  (best-size (if (retina canvas) (convert-size-to-backing canvas)
			       (list (ns:width canvas) (ns:height canvas))))
		  (renderer (renderer canvas)))
	      (setf scene-size (get-scene-size scene-ratio best-size))
	      (setf (view-size canvas) best-size)
	      (setf (width renderer) (car scene-size)
		      (height renderer) (second scene-size))
	      (resize-framebuffer renderer (width renderer) (height renderer)))
	  (let* ((scene-ratio (getf options :scene-ratio))
		 (syphon (getf options :syphon)))
	    (when (or (/= (scene-ratio canvas) scene-ratio)
		      (not (eql (retina canvas) (getf options :retina))))
	      (setf (retina canvas) (getf options :retina))
	      (ns:objc canvas "setWantsBestResolutionOpenGLSurface:" :bool (retina canvas))
	      (let* ((best-size (if (retina canvas) (convert-size-to-backing canvas)
				  (list (ns:width canvas) (ns:height canvas)))))
		(setf (view-size canvas) best-size)
		(setf (scene-ratio canvas) scene-ratio
		      scene-size (get-scene-size scene-ratio best-size))))
	    (reinit-visual-renderer (renderer canvas) options scene-size)
	    (cond ((and syphon (not (syphon canvas))) (setf (syphon canvas)
							(syphon:make-server "LispSystem"
									    (ns:cgl-context canvas))))
		  ((and (not syphon) (syphon canvas)) (let ((syphon (syphon canvas)))
							(syphon:stop-server syphon)
							(ns:release syphon)
							(setf (syphon canvas) nil))))
	    (setf (info canvas) (getf options :info)
		  (user-fn canvas) (getf options :user-fn)
		  (output-filter canvas) (getf options :output-filter)))))
      	scene-size)))

(defun apply-filter (canvas width height)
  (let* ((ciimage (ci:image-from-texture (texture canvas) (ns:make-size width height)))
	 (rect (ns:make-rect 0 0 width height)))
    (loop for filter in (output-filter canvas)
	  do (setf ciimage (ci:apply-filter filter ciimage)))
    (gl:bind-framebuffer :framebuffer (framebuffer canvas))
    (gl:viewport 0 0 width height)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0 width 0 height -100.0 100.0)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (ci:draw-image (ci-context canvas) ciimage rect rect)
    (gl:bind-framebuffer :framebuffer 0)))


(defmethod ns:draw ((view visual-canvas))
  (when-let ((user-fn (user-fn view)))
    (funcall user-fn))
  (let* ((ctx (cgl:get-current-context)))
    (let* ((size-update-p (update-visual-canvas view)))
      (render (renderer view))
      (let* ((w (width (renderer view)))
  	     (h (height (renderer view)))
  	     (view-w (car (view-size view)))
  	     (view-h (second (view-size view))))
  	(when size-update-p
  	  (resize-framebuffer view w h))
  	(when (output-filter view)
  	  (apply-filter view w h))
  	(gl:viewport 0 0 view-w view-h)
  	(gl:clear-color .0 .0 .0 1.0)
  	(gl:clear :color-buffer-bit)
  	(gl:bind-texture :texture-rectangle (texture view))
  	(draw-framebuffer view :triangles 0 6 (frame-stream view)
  	  		       :image 0
  	  		       :image-resolution (scene-ratio view)
  	  		       :iresolution (list view-w view-h))
  	(gl:bind-texture :texture-rectangle 0)
  	(when (syphon view)
  	  (syphon:publish-frame (syphon view) (texture view)  
  	  			(cffi:foreign-enum-value '%gl:enum :texture-rectangle)
				(ns:make-rect 0 0 w h)
				(ns:make-size w h)))
	(let* ((now (gfx:get-internal-seconds)))
  	  (calc-fps-info (fps-info view) (- now (last-draw-time view)))
	  (setf (last-draw-time view) now))
  	(when (info view)
  	  (draw-fps-info (fps-info view) view-w view-h w h))))))



(defvar *visual-canvas* nil)
(defvar *last-commands* nil)
(defvar *last-ichannel-targets* nil)

(defmethod ns:release ((view visual-canvas))
  (release (renderer view))
  (ns:release (iosurface view))
  (ns:release (ci-context view))
  (destroy-fps-info (fps-info view))
  (gl:delete-texture (texture view))
  (gl:delete-renderbuffers (list (depthbuffer view)))
  (gl:delete-framebuffers (list (framebuffer view)))
  (when-let ((syphon (syphon view)))
    (syphon:stop-server syphon)
    (ns:release syphon))
  (sc:free (audio-group view))
  (setf *visual-canvas* nil)
  (setf *last-commands* nil))

(defmacro gfx::start-shader (shader &key textures
				      (reinit-time (let ((cur-time (gfx:get-internal-seconds)))
						     (lambda () (- (gfx:get-internal-seconds) cur-time))))
				      size
				      (scene-ratio 1)
				      user-fn
				      syphon
				      output-filter
				      retina
				      (info t)
				      gl-canvas)
  (let* ((window-name (format nil "~a" shader)))
    (assert (gethash shader gfx::*all-pipeline-table*) nil "can't find \"~a\" shader" shader)
    (setf *last-commands* (list :textures textures :reinit-time reinit-time :size size
			       :scene-ratio scene-ratio :user-fn user-fn :syphon syphon :output-filter output-filter
			       :retina retina :info info :gl-canvas gl-canvas))
    `(if *visual-canvas* (progn (send-message (mailbox *visual-canvas*)
					      (list :textures ,textures
						    :shader ',shader
						    :output-filter ,output-filter
						    :syphon ,syphon
						    :user-fn ,user-fn
						    :info ,info
						    :scene-ratio ,scene-ratio
						    :retina ,retina
						    :gl-canvas ,gl-canvas))
				(ns:with-event-loop nil
				  (ns:objc (window *visual-canvas*) "setTitle:"
					   :pointer (ns:autorelease (ns:make-ns-string ,window-name)))
				  (when (and ,size (not (ns:objc (cl-visual::window cl-visual::*visual-canvas*) "isFullscreen" :bool)))
				    (let* ((window (window *visual-canvas*))
					   (frame (ns:objc-stret ns:rect window "frame")))
				      (ns:objc (window *visual-canvas*) "setFrame:display:"
					       (:struct ns:rect) (ns:make-rect (ns:rect-x frame)
									       (+ (ns:rect-y frame)
										  (- (ns:rect-height frame)
										     (+ 22 ,(third size))))
									       ,(second size)
									       (+ 22 ,(third size)))
					       :int 0)))))
       (ns:with-event-loop (:waitp t)
	 (let* ((renderer (make-instance 'visual-renderer :reinit-time ,reinit-time
	 				 :core-profile t))
	 	(canvas (make-instance 'visual-canvas :x 0 :y 0 :w ,(if size (second size) 720) :h ,(if size (third size) 450)
	 			       :scene-ratio ,scene-ratio
	 			       :renderer renderer
	 			       :retina ,retina
	 			       :core-profile nil))
	 	(window (make-instance 'ns:window :x 0 :y 1000 :w ,(if size (second size) 720) :h ,(if size (third size) 450)
	 			       :title ,window-name)))
	   (ns:objc canvas "setWantsBestResolutionOpenGLSurface:" :bool (retina canvas))
	   (setf (ns:content-view window) canvas)
	   (setf (window canvas) window)
	   (send-message (mailbox canvas) :force-resize)
	   (send-message (mailbox canvas)
	   		 (list :textures ,textures
	   		       :shader ',shader
	   		       :output-filter ,output-filter
	   		       :syphon ,syphon
	   		       :user-fn ,user-fn
	   		       :info ,info
	   		       :scene-ratio ,scene-ratio
	   		       :retina ,retina
	   		       :gl-canvas ,gl-canvas))
	   (setf *visual-canvas* canvas)
	   (ns:window-show window))))))


(defmacro gfx::define-shader (name &body body)
  (let ((name (ensure-list name))
	(ichannel-targets (make-list 8 :initial-element :sampler-2d-rect)))
    (loop for (index target) in (second name)
	  do (setf (nth index ichannel-targets) target))
    `(progn
       (gfx:defpipeline (,(car name) :version 330)
	   (,@(loop for i from 0 below 8
		    collect (list (intern (format nil "ICHANNEL~d" i)) (nth i ichannel-targets)))
	    (iglobal-time :float)
	    (itime :float)
	    ,@(loop for i from 0 below 6
		    collect (list (intern (format nil "IVOLUME~d" i)) :float))
	    ,@(loop for i from 0 below 10
		    collect (list (intern (format nil "ICONTROL~d" i)) :float))
	    (iresolution :vec2)
	    (camera :vec3)
	    (lookat :vec3)
	    (projection-matrix :mat4)
	    (modelview-matrix :mat4))
	 (:vertex ((pos :vec2))
		  (values
		   (v! pos 0.0 1.0)
		   pos))
	 (:fragment ((vfuv :vec2))
		    (progn ,@body)))
       ;; reinterpret ======================================================================
       (when (and *visual-canvas*
		  (eql ',(car name) (shader (renderer *visual-canvas*)))
		  (not (equal ',ichannel-targets *last-ichannel-targets*)))
	 (gfx::start-shader ,(car name) ,@*last-commands*)
	 (format t "~&reinterpret shader: ~a~%" ',(car name)))
       (setf *last-ichannel-targets* ',ichannel-targets)
       ',(car name))))


(defmethod ns:mouse-wheel ((view visual-canvas) event localtion-x location-y)
  (declare (ignore location-x location-y))
  (let* ((x (float (ns:objc event "deltaX" :double) 1.0))
  	 (y (float (ns:objc event "deltaY" :double) 1.0))
  	 (camera (camera (renderer view))))
    (if (> .004 (abs x)) (setf x 0.0))
    (if (> .004 (abs y)) (setf y 0.0))
    (cond ((ns:ctrl-p event) (gfx:track-mouse-zoom camera  (- x) (- y) .1))
    	  ((ns:shift-p event) (gfx:track-mouse-pan camera (- x) y .1))
    	  (t (gfx:track-mouse-spin camera x (- y) .1)))))

(defun gfx::reset-visual-camera (&key (eye-x 0.0) (eye-y 0.0) (eye-z 5.0)
				      (center-x 0.0) (center-y 0.0) (center-z 0.0))
  (when *visual-canvas*
    (gfx:reset-camera (camera (renderer *visual-canvas*))
		      :eye-x eye-x :eye-y eye-y :eye-z eye-z
		      :center-x center-x :center-y center-y :center-z center-z)
    t))


(gfx::clear-pipeline)

(export '(gfx::define-shader gfx::start-shader gfx::clear-pipeline gfx::reset-visual-camera) :gfx)


