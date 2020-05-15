(in-package :post-fx)

(gfx:defpipeline draw-fx ((source :sampler-2d-rect) (effect :sampler-2d-rect))
  (:vertex ((pos :vec2) (coord :vec2))
	   (values
	    (v! pos 0.0 1.0)
	    coord))
  (:fragment ((c :vec2))
	     (+
	      (sl:texture! source c)
	      (sl:texture! effect c))))

(gfx:defpipeline bloom-alpha-filter ((tex :sampler-2d-rect) (threshold :float))
  (:vertex ((pos :vec2) (coord :vec2))
	   (values
	    (v! pos 0.0 1.0)
	    coord))
  (:fragment ((c :vec2))
	     (let* ((color (sl:texture! tex c))
		    (alpha (w color)))
	       (if (> alpha threshold) (v! (xyz color) 1.0)
		 (vec4 0.0)))))

(gfx:defpipeline bloom-color-filter ((tex :sampler-2d-rect) (threshold :float))
  (:vertex ((pos :vec2) (coord :vec2))
	   (values
	    (v! pos 0.0 1.0)
	    coord))
  (:fragment ((c :vec2))
	     (let* ((color (sl:texture! tex c)))
	       (if (> (max (x color) (max (y color) (z color))) threshold)
		   color
		 (vec4 0.0)))))

(gfx:defpipeline bloom ((tex :sampler-2d-rect) (horizontal :int) (offset :float) (power :float))
  (:vertex ((pos :vec2) (coord :vec2))
	   (values
	    (v! pos 0.0 1.0)
	    coord))
  (:fragment ((c :vec2))
	     (let* ((weight (make-array :float 5)))
	       (setf (aref weight 0) (* 0.3027d0 power)
		     (aref weight 1) 0.195946d0
		     (aref weight 2) 0.1216216d0
		     (aref weight 3) 0.054054d0
		     (aref weight 4) 0.016216d0)
	       (let* ((tex-offset (/ offset (texture-size tex)))
		      (result (* (xyz (sl:texture! tex c)) (aref weight 0))))
		 (if (= horizontal 1)
		     (loop for i from 1 below 5
			   do (incf result (* (xyz (sl:texture! tex (+ c (v! (* i (x tex-offset)) 0.0))))
					      (aref weight i)))
			      (incf result (* (xyz (sl:texture! tex (- c (v! (* i (x tex-offset)) 0.0))))
					      (aref weight i)) ))
		   (loop for i from 1 below 5
			 do (incf result (* (xyz (sl:texture! tex (+ c (v! 0.0 (* i (y tex-offset))))))
					    (aref weight i)))
			    (incf result (* (xyz (sl:texture! tex (- c (v! 0.0 (* i (y tex-offset))))))
					    (aref weight i)))))
		 (v! result 1.0)))))


(defclass bloom (gfx:gl-canvas)
  ((gpu-stream :initform (gfx:make-gpu-stream '((pos :vec2) (coord :vec2))
					      (list -1.0 -1.0 0.0 0.0
						    1.0 -1.0 1.0 0.0
						    -1.0  1.0 0.0 1.0
						    -1.0  1.0 0.0 1.0
						    1.0 -1.0 1.0 0.0
						    1.0  1.0 1.0 1.0))
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
	(:alpha (bloom-alpha-filter view :triangles 0 6 (gpu-stream view) :tex 0 :threshold threshold))
	(t (bloom-color-filter view :triangles 0 6 (gpu-stream view) :tex 0 :threshold threshold))))
    (gl:active-texture :texture1)
    (gfx:with-fbo ((bloom-fbo view))
      (gl:bind-texture :texture-rectangle (gfx:output-texture (filter-fbo view)))
      (bloom view :triangles 0 6 (gpu-stream view) :tex 1 :horizontal 1 :offset offset :power power))
    (dotimes (i (floor repeat))
      (gfx:with-fbo ((filter-fbo view))
	(gl:bind-texture :texture-rectangle (gfx:output-texture (bloom-fbo view)))
	(bloom view :triangles 0 6 (gpu-stream view) :tex 1 :horizontal 0 :offset offset :power power))
      (gfx:with-fbo ((bloom-fbo view))
	(gl:bind-texture :texture-rectangle (gfx:output-texture (filter-fbo view)))
	(bloom view :triangles 0 6 (gpu-stream view) :tex 1 :horizontal 1 :offset offset :power power)))
    (gl:bind-texture :texture-rectangle (gfx:output-texture (bloom-fbo view)))
    (draw-fx view :triangles 0 6 (gpu-stream view) :source 0 :effect 1)))






