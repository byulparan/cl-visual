(in-package :post-fx)

(gfx:defpipeline draw-fx ((source :sampler-2d-rect) (effect :sampler-2d-rect))
  (:vertex (:in ((pos :vec2) (coord :vec2))
	    :out ((v-coord :vec2)))
	   (progn
	     (setf v-coord coord)
	     (v! pos 0.0 1.0)))
  (:fragment (:in ((v-coord :vec2)))
	     (+
	      (sl:texture! source v-coord)
	      (sl:texture! effect v-coord))))

(gfx:defpipeline bloom-alpha-filter ((tex :sampler-2d-rect) (threshold :float))
  (:vertex (:in ((pos :vec2) (coord :vec2))
	    :out ((v-coord :vec2)))
	   (progn
	     (setf v-coord coord)
	     (v! pos 0.0 1.0)))
  (:fragment (:in ((v-coord :vec2)))
	     (let* ((color (sl:texture! tex v-coord))
		    (alpha (w color)))
	       (if (> alpha threshold) (v! (xyz color) 1.0)
		 (vec4 0.0)))))

(gfx:defpipeline bloom-color-filter ((tex :sampler-2d-rect) (threshold :float))
  (:vertex (:in ((pos :vec2) (coord :vec2))
	    :out ((v-coord :vec2)))
	   (progn
	     (setf v-coord coord)
	     (v! pos 0.0 1.0)))
  (:fragment (:in ((v-coord :vec2)))
	     (let* ((color (sl:texture! tex v-coord)))
	       (if (> (max (x color) (max (y color) (z color))) threshold)
		   color
		 (vec4 0.0)))))

(gfx:defpipeline bloom ((tex :sampler-2d-rect) (horizontal :int) (offset :float) (power :float))
  (:vertex (:in ((pos :vec2) (coord :vec2))
	    :out ((v-coord :vec2)))
	   (progn
	     (setq v-coord coord)
	     (v! pos 0.0 1.0)))
  (:fragment (:in ((v-coord :vec2)))
	     (let* ((weight (make-array :float 5)))
	       (setf (aref weight 0) (* 0.3027d0 power)
		     (aref weight 1) 0.195946d0
		     (aref weight 2) 0.1216216d0
		     (aref weight 3) 0.054054d0
		     (aref weight 4) 0.016216d0)
	       (let* ((tex-offset (/ offset (texture-size tex)))
		      (result (* (xyz (sl:texture! tex v-coord)) (aref weight 0))))
		 (if (= horizontal 1)
		     (loop for i from 1 below 5
			   do (incf result (* (xyz (sl:texture! tex (+ v-coord (v! (* i (x tex-offset)) 0.0))))
					      (aref weight i)))
			      (incf result (* (xyz (sl:texture! tex (- v-coord (v! (* i (x tex-offset)) 0.0))))
					      (aref weight i)) ))
		   (loop for i from 1 below 5
			 do (incf result (* (xyz (sl:texture! tex (+ v-coord (v! 0.0 (* i (y tex-offset))))))
					    (aref weight i)))
			    (incf result (* (xyz (sl:texture! tex (- v-coord (v! 0.0 (* i (y tex-offset))))))
					    (aref weight i)))))
		 (v! result 1.0)))))


(defclass bloom (gfx:gl-canvas)
  ((gpu-stream :initform (gfx:make-gpu-stream '((pos :vec2) (coord :vec2))
					      (list -1.0 -1.0 0.0 0.0
						    1.0 -1.0 1.0 0.0
						    -1.0  1.0 0.0 1.0
						    -1.0  1.0 0.0 1.0
						    1.0 -1.0 1.0 0.0
						    1.0  1.0 1.0 1.0)
					      :core-profile t)
	       :reader gpu-stream)
   (filter-fbo :accessor filter-fbo)
   (bloom-fbo :accessor bloom-fbo)
   (params :initform (list :filter :alpha :threshold .3 :power 1.0 :offset 1.0 :repeat 4) :reader params)))

(defun set-bloom (bloom &key filter threshold power offset repeat)
  (let* ((params (params bloom)))
    (when filter (setf (getf params :filter) filter))
    (when threshold (setf (getf params :threshold) threshold))
    (when power (setf (getf params :power) power))
    (when offset (setf (getf params :offset) offset))
    (when repeat (setf (getf params :repeat) (floor repeat)))
    (values)))


(defmethod gfx:init :around ((view bloom))
  (setf (filter-fbo view) (gfx:make-fbo (gfx:width view) (gfx:height view)
					:target :texture-rectangle))
  (setf (bloom-fbo view) (gfx:make-fbo (gfx:width view) (gfx:height view)
					:target :texture-rectangle))
  (call-next-method))

(defmethod gfx:release ((view bloom))
  (gfx:release-fbo (filter-fbo view))
  (gfx:release-fbo (bloom-fbo view)))

(defmethod gfx:draw :around ((view bloom))
  (call-next-method)
  (let* ((w (gfx:width view))
	 (h (gfx:height view))
	 (params (params view))
	 (threshold (getf params :threshold))
	 (offset (getf params :offset))
	 (power (getf params :power))
	 (repeat (getf params :repeat)))
    (gl:viewport 0 0 w h)
    (when (or (/= (gfx:width (filter-fbo view)) w)
	      (/= (gfx:height (filter-fbo view)) h))
      (gfx:reinit-fbo (filter-fbo view) w h)
      (gfx:reinit-fbo (bloom-fbo view) w h))
    (gfx:with-fbo ((filter-fbo view))
      (case (getf params :filter)
	(:alpha
	 (gfx:with-shader (view 'bloom-alpha-filter (gpu-stream view))
	   (gfx:set-uniform 'tex 0)
	   (gfx:set-uniform 'threshold threshold)
	   (gl:draw-arrays :triangles 0 6)))
	(t (gfx:with-shader (view 'bloom-color-filter (gpu-stream view))
	     (gfx:set-uniform 'tex 0)
	     (gfx:set-uniform 'threshold threshold)
	     (gl:draw-arrays :triangles 0 6)))))
    (gl:active-texture :texture1)
    (gfx:with-fbo ((bloom-fbo view))
      (gl:bind-texture :texture-rectangle (gfx:output-texture (filter-fbo view)))
      (gfx:with-shader (view 'bloom (gpu-stream view))
	(gfx:set-uniform 'tex 1)
	(gfx:set-uniform 'horizontal 1)
	(gfx:set-uniform 'offset offset)
	(gfx:set-uniform 'power power)
	(gl:draw-arrays :triangles 0 6)))
    (dotimes (i (floor repeat))
      (gfx:with-fbo ((filter-fbo view))
	(gl:bind-texture :texture-rectangle (gfx:output-texture (bloom-fbo view)))
	(gfx:with-shader (view 'bloom (gpu-stream view))
	  (gfx:set-uniform 'tex 1)
	  (gfx:set-uniform 'horizontal 0)
	  (gfx:set-uniform 'offset offset)
	  (gfx:set-uniform 'power power)
	  (gl:draw-arrays :triangles 0 6)))
      (gfx:with-fbo ((bloom-fbo view))
	(gl:bind-texture :texture-rectangle (gfx:output-texture (filter-fbo view)))
	(gfx:with-shader (view 'bloom (gpu-stream view))
	  (gfx:set-uniform 'tex 1)
	  (gfx:set-uniform 'horizontal 1)
	  (gfx:set-uniform 'offset offset)
	  (gfx:set-uniform 'power power)
	  (gl:draw-arrays :triangles 0 6))))
    (gl:bind-texture :texture-rectangle (gfx:output-texture (bloom-fbo view)))
    (gfx:with-shader (view 'draw-fx (gpu-stream view))
      (gfx:set-uniform 'source 0)
      (gfx:set-uniform 'effect 1)
      (gl:draw-arrays :triangles 0 6))))






