(in-package :cl-visual)

(defun gfx::add-uniform (name type)
  (setf (gethash name gfx::*gfx-uniform-table*) (glsl::make-code-object type (ppcre:regex-replace-all "-" (string-downcase name) "_"))))

(defun gfx::clear-pipeline (&optional remove-uniforms)
  (gfx:reinit-shader-system)
  (loop for pipeline being the hash-values of gfx::*all-pipeline-table*
 	do (setf (gfx::%pipeline-used-funcs pipeline) nil))
  (unless remove-uniforms 
    (loop for chan in '(ichannel0 ichannel1 ichannel2 ichannel3 ichannel4 ichannel5
			ichannel6 ichannel7)
	  do (gfx::add-uniform chan :sampler-2d))
    (loop for cont in '(icontrol0 icontrol1 icontrol2 icontrol3 icontrol4
			icontrol5 icontrol6 icontrol7 icontrol8 icontrol9)
	  do (gfx::add-uniform cont :float))
    (loop for vol in '(ivolume0 ivolume1 ivolume2 ivolume3 ivolume4 ivolume5)
	  do (gfx::add-uniform vol :float))
    (gfx::add-uniform 'iglobal-time :float)
    (gfx::add-uniform 'itime :float)
    (gfx::add-uniform 'iresolution :vec2)
    (values)))



(gfx:defpipeline (draw-framebuffer :version 120) ((image :sampler-2d-rect)
						  (image-resolution :float)
						  (gfx::iresolution :vec2))
  (:vertex (:in ((pos :vec2) (coord :vec2))
	    :out ((v-coord :vec2)))
	   (progn
	     (setf v-coord coord)
	     (v! pos .0 1.0)))
  (:fragment (:in ((v-coord :vec2)))
	     (texture image (* v-coord gfx::iresolution image-resolution))))

(defclass visual-canvas (ns:opengl-view gfx:shader-environment)
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
    :accessor last-draw-time)
   (use-mouse
    :initarg :use-mouse
    :accessor use-mouse)))



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
    (unless (framebuffer renderer)
      (setf (framebuffer renderer) (gl:gen-framebuffer)
	    (texture renderer) (gl:gen-texture)
	    (depthbuffer renderer) (gl:gen-renderbuffer)))
    (make-framebuffer-from-iosurface cgl-context (framebuffer renderer) (texture renderer) (depthbuffer renderer)
				     (iosurface (renderer renderer)) width height)))


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
      (#+sbcl send-message
       #-sbcl mailbox-send-message
       (mailbox view) :force-resize))))

(defun update-visual-canvas (canvas)
  (unless (mailbox-empty-p (mailbox canvas))
    (let* ((options (#+sbcl receive-message
		     #-sbcl mailbox-receive-message
		     (mailbox canvas)))
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

(defmethod ns:release ((view visual-canvas))
  (release (renderer view))
  (gfx:release-environment view)
  (ns:release (ci-context view))
  (destroy-fps-info (fps-info view))
  (gl:delete-texture (texture view))
  (gl:delete-renderbuffers (list (depthbuffer view)))
  (gl:delete-framebuffers (list (framebuffer view)))
  (when-let ((syphon (syphon view)))
    (syphon:stop-server syphon)
    (ns:release syphon))
  (sc:free (audio-group view))
  (setf *visual-canvas* nil))


(defmacro gfx::define-shader (name &body body)
  `(progn
       (gfx:defpipeline (,name :version 330)
	   (,@(loop with pipeline = (gethash name gfx::*all-pipeline-table*)
		    for i from 0 below 8
		    for uniform in (if pipeline (gfx::%pipeline-uniforms pipeline)
				     (make-list 8))
		    collect (list (intern (format nil "ICHANNEL~d" i))
				  (if pipeline (second uniform)
				      :sampler-2d)))
	    (iglobal-time :float)
	    (itime :float)
	    ,@(loop for i from 0 below *num-ivolume*
		    collect (list (intern (format nil "IVOLUME~d" i)) :float))
	    ,@(loop for i from 0 below *num-icontrol*
		    collect (list (intern (format nil "ICONTROL~d" i)) :float))
	    (iresolution :vec2)
	    (camera :vec3)
	    (lookat :vec3)
	    (projection-matrix :mat4)
	    (modelview-matrix :mat4)
	    (imouse :vec3))
	 (:vertex (:in ((pos :vec2))
		   :out ((vfuv :vec2)))
		  (progn
		    (setq vfuv pos)
		    (v! pos 0.0 1.0)))
	 (:fragment (:in ((vfuv :vec2)))
		    (progn ,@body)))
       ',name))


(defmacro gfx::start-shader (shader &key textures (reinit-time (let ((cur-time (gfx:get-internal-seconds)))
								 (lambda () (- (gfx:get-internal-seconds) cur-time))))
				      size (scene-ratio 1) user-fn (use-mouse t) syphon output-filter retina
				      (info t) gl-canvas multisample)
  (with-gensyms (window-name message)
    `(let* ((,window-name (format nil "~a" ',shader))
	    (,message (list :shader ',shader
			    :textures ,textures
			    :scene-ratio ,scene-ratio :user-fn ,user-fn :syphon ,syphon :output-filter ,output-filter
			    :retina ,retina :info ,info :gl-canvas ,gl-canvas :multisample ,multisample)))
       (assert (gethash ',shader gfx::*all-pipeline-table*) nil "can't find \"~a\" shader" ',shader)
       (if *visual-canvas* (progn (#+sbcl send-message
				   #-sbcl mailbox-send-message
				   (mailbox *visual-canvas*)
				   ,message)
				  (setf (use-mouse *visual-canvas*) ,use-mouse)
				  (ns:with-event-loop nil
				    (ns:objc (window *visual-canvas*) "setTitle:"
					     :pointer (ns:autorelease (ns:make-ns-string ,window-name)))
				    (when (and ,size (not (ns:objc (cl-visual::window cl-visual::*visual-canvas*) "isFullscreen" :bool)))
				      (let* ((window (window *visual-canvas*))
					     (frame #+x86-64 (ns:objc-stret ns:rect window "frame")
						    #+arm64 (ns:objc window "frame" (:struct ns:rect))))
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
					   :use-mouse ,use-mouse
	 				   :scene-ratio ,scene-ratio
	 				   :renderer renderer
	 				   :retina ,retina
	 				   :core-profile nil))
	 	    (window (make-instance 'ns:window
			      :rect (ns:in-screen-rect (ns:make-rect 0 1000 ,(if size (second size) 720) ,(if size (third size) 450)))
			      :title ,window-name)))
	       (ns:objc canvas "setWantsBestResolutionOpenGLSurface:" :bool (retina canvas))
	       (setf (ns:content-view window) canvas)
	       (setf (window canvas) window)
	       (#+sbcl send-message
		#-sbcl mailbox-send-message 
		(mailbox canvas) :force-resize)
	       (#+sbcl send-message
		#-sbcl mailbox-send-message
		(mailbox canvas)
		,message)
	       (setf *visual-canvas* canvas)
	       (ns:window-show window)))))))


(defmethod ns:mouse-wheel ((view visual-canvas) event location-x location-y)
  (declare (ignorable location-x location-y))
  (when (use-mouse view)
    (let* ((x (float (ns:objc event "deltaX" :double) 1.0))
  	   (y (float (ns:objc event "deltaY" :double) 1.0))
  	   (camera (camera (renderer view))))
      (if (> .004 (abs x)) (setf x 0.0))
      (if (> .004 (abs y)) (setf y 0.0))
      (cond ((ns:ctrl-p event) (gfx:track-mouse-zoom camera  (- x) (- y) .1))
    	    ((ns:shift-p event) (gfx:track-mouse-pan camera (- x) y .1))
    	    (t (gfx:track-mouse-spin camera x (- y) .1))))))


(defmethod ns:mouse-moved ((view visual-canvas) event location-x location-y)
  (declare (ignorable event))
  (when (use-mouse view)
    (setf (imouse (renderer view)) (list location-x location-y (nth 2 (imouse (renderer view)))))))

(defmethod ns:mouse-down ((view visual-canvas) event location-x location-y)
  (declare (ignorable event location-x location-y))
  (when (use-mouse view)
    (setf (nth 2 (imouse (renderer view))) 1.0)))

(defmethod ns:mouse-up ((view visual-canvas) event location-x location-y)
  (declare (ignorable event location-x location-y))
  (when (use-mouse view)
    (setf (nth 2 (imouse (renderer view))) 0.0)))


(defun gfx::reset-visual-camera (&key (eye-x 0.0) (eye-y 0.0) (eye-z 5.0)
				      (center-x 0.0) (center-y 0.0) (center-z 0.0))
  (when *visual-canvas*
    (gfx:reset-camera (camera (renderer *visual-canvas*))
		      :eye-x eye-x :eye-y eye-y :eye-z eye-z
		      :center-x center-x :center-y center-y :center-z center-z)
    t))



(defun gfx::toggle-fullscreen ()
  (ns:with-event-loop nil
    (when *visual-canvas*
      (ns:toggle-fullscreen (ns::cocoa-ref (window *visual-canvas*))))))

(defun gfx::shader-close ()
  (ns:with-event-loop nil
    (when *visual-canvas*
      (ns:window-close (ns::cocoa-ref (window *visual-canvas*))))))


(gfx::clear-pipeline)

(export '(gfx::define-shader gfx::start-shader gfx::toggle-fullscreen gfx::shader-close gfx::clear-pipeline gfx::add-uniform gfx::reset-visual-camera) :gfx)


