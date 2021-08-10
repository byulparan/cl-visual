(defpackage :shader-lib
  (:shadowing-import-from #:gfx #:defpipeline :fill)
  (:nicknames :sl)
  (:use #:cl :glsl :cl-visual :alexandria)
  (:export #:build-light))

(in-package :shader-lib)

(defmacro define-function-library (name args &body body)
  (let ((newargs (mapcar #'(lambda (a) (intern (format nil "~a-~a" (first a) (second a)))) args)))
    `(progn
       (let* ((gfx::*gfx-spec-table* gfx::*library-spec-table*)
	      (gfx::*gfx-function-table* gfx::*library-function-table*))
	 (gfx:defun-g ,name ,args
	   ,@body)
	 (export ',name))
       (defun ,name ,newargs
	 (declare (ignore ,@newargs))))))

(defmacro define-macro-library (name args &body body)
  `(progn
     (defmacro ,name ,args
       ,@body)
     (let* ((glsl::*macro-table* gfx::*library-macro-table*))
       (glsl:v-defmacro ,name ,args
	 ,@body))
     (export ',name)))

(define-macro-library with-uv ((uv &optional (normal-uv 'uvn)) &body body)
  `(let* ((,normal-uv (/ (xy gl-frag-coord) (xy iresolution)))
	  (,uv (- (* ,normal-uv 2.0) 1.0)))
     (setf (x ,uv) (* (x ,uv) (/ (x iresolution) (y iresolution))))
     ,@body))

(define-macro-library do-repeat ((var n) &body body)
  (cons 'progn
	(loop for i from 0 below n
	      collect `(let ((,var ,i))
			 ,@body))))

(define-macro-library video-texture (texture uvm)
  `(let ((new-color (texture ,texture (v! (x ,uvm) (- 1.0 (y ,uvm))))))
     (v! (y new-color) (z new-color) (w new-color) 1.0)))

(define-macro-library with-camera ((rd-var uv ro ta &optional (cp '(v! 0.0 1.0 0.0)) (dist 2.0)) &body body)
  (with-gensyms (cw cu cv var)
    `(let* ((,cw (normalize (- ,ta ,ro)))
	    (,cu (normalize (cross ,cw ,cp)))
	    (,cv (normalize (cross ,cu ,cw)))
	    (,var (m! ,cu ,cv ,cw))
	    (,rd-var (* ,var (normalize (v! ,uv ,dist)))))
       ,@body)))

(define-macro-library with-raymarch ((uv-var rd-var ro ta &optional (uvn-var 'uvn) (dist 2.0)) &body body)
  `(with-uv (,uv-var ,uvn-var)
     (let* (,ro
	    ,ta)
       (with-camera (,rd-var ,uv-var ,(car ro) ,(car ta) (v! .0 1.0 .0) ,dist)
	 ,@body))))

(define-macro-library with-hybrid ((ro rd depth) &body body)
  `(let* ((uv vfuv)
	  (,ro (let* ((ro (- (xyz (aref modelview-matrix 3)))))
		 (incf (x ro) (x (aref projection-matrix 2)))
		 ro))
	  (,rd (let* ((aspect (/ (x iresolution) (y iresolution)))
		      (u-fovy (/ 1.0 (y (aref projection-matrix 1))))
		      (dir (v! (* (x uv) u-fovy  aspect)
			       (* (y uv) u-fovy)
			       -1.0)))
		 (normalize dir)))
	  (,depth 9999.0))
     (incf (x ,rd) (x (aref projection-matrix 2)))
     (setf ,rd (normalize ,rd))
     (setf ,ro (* ,ro (mat3 modelview-matrix)))
     (setf ,rd (* ,rd (mat3 modelview-matrix)))
     ,@body
     (let* ((eye-fwd (* (v! 0.0 0.0 -1.0) (mat3 modelview-matrix)))
	    (eye-hit-z (* (- ,depth) (dot ,rd eye-fwd)))
	    (p10 (z (aref projection-matrix 2)))
	    (p11 (z (aref projection-matrix 3)))
	    (ndc-depth (+ (- p10) (/ (- p11) eye-hit-z)))
	    (dep (/ (+ (* (s~ gl-depth-range "diff" :float) ndc-depth)
		       (s~ gl-depth-range "near" :float)
		       (s~ gl-depth-range "far" :float))
		    2.0)))
       (setf gl-frag-depth dep))))



(define-function-library internal-op ((d1 :float) (d2 :float))
  (let* ((result 0.0))
    (if! (< d1 d2) (setf result d1)
	 (setf result d2))
    result))

(unexport 'internal-op)

(define-macro-library op-u (d1 &body body)
  (if body (let* ((d2 (car body))
		  (item `(internal-op ,d1 ,d2)))
	     (loop for b in (cdr body)
		   do (setf item `(internal-op ,item ,b)))
	     item)
    d1))

(define-function-library internal-op2 ((d1 :vec2) (d2 :vec2))
  (let* ((result (v! 0.0 0.0)))
    (if! (< (x d1) (x d2)) (setf result d1)
	 (setf result d2))
    result))

(unexport 'internal-op2)

(define-macro-library op-u2 (d1 &body body)
  (if body (let* ((d2 (car body))
		  (item `(internal-op2 ,d1 ,d2)))
	     (loop for b in body
		   do (setf item `(internal-op2 ,item ,b)))
	     item)
    d1))

(define-function-library internal-op-s ((d1 :float) (d2 :float))
  (let* ((result 0.0))
    (if! (> (- d2) d1)
	 (setf result (- d2))
	 (setf result d1))
    result))

(unexport 'internal-op-s)

(define-macro-library op-s (d1 &body body)
  (if body (let* ((d2 (car body))
		  (item `(internal-op-s ,d1 ,d2)))
	     (loop for b in (cdr body)
		   do (setf item `(internal-op-s ,item ,b)))
	     item)
    d1))


(define-function-library op-s2 ((d1 :vec2) (d2 :vec2))
  (let* ((result (v! 0.0 0.0)))
    (if! (> (- (x d1)) (x d2))
	 (setf result (v! (- (x d1)) (y d1)))
	 (setf result d2))
    result))

(define-function-library sd-plane ((p :vec3) (n :vec4))
  (+ (dot p (xyz n)) (w n)))

(define-function-library sd-base ((p :vec3))
  (y p))

(define-function-library sd-box ((p :vec3) (b :vec3))
  (let* ((d (- (abs p) b)))
    (+ (min (max (x d) (max (y d) (z d))) 0.0) (length (max d 0.0)))))

(define-function-library sd-sphere ((p :vec3) (s :float))
  (- (length p) s))

(define-function-library sd-cylinder ((p :vec3) (a :vec3) (b :vec3) (r :float))
  (let* ((ab (- b a))
	 (ap (- p a))
	 (t (/ (dot ab ap) (dot ab ab))))
    (let* ((c (+ a (* t ab)))
	   (x (- (length (- p c)) r))
	   (y (* (- (abs (- t .5)) .5) (length ab)))
	   (e (length (max (v! x y) 0.0)))
	   (i (min (max x y) 0.0)))
      (+ e i))))

(define-function-library sd-torus ((p :vec3) (r :vec2))
  (let* ((x (- (length (xz p)) (x r))))
    (- (length (v! x (y p))) (y r))))

(define-function-library sd-capture ((p :vec3) (a :vec3) (b :vec3) (r :float))
  (let* ((ab (- b a))
	 (ap (- p a))
	 (t (/ (dot ab ap) (dot ab ab))))
    (setf t (clamp t .0 1.0))
    (let* ((c (+ a (* t ab))))
      (- (length (- p c)) r))))



(define-function-library flip ((uv :vec2))
  (v! (x uv) (- 1.0 (y uv))))

(define-macro-library get-data (texture indx)
  `(x (texture ,texture (v! ,indx .0))))

(define-function-library syphon-ratio ((uvn :vec2) (iresolution :vec2) (width :float) (height :float) (x-ratio :float) (h-ratio :float))
  (let* ((w (* (x iresolution) x-ratio (/ height (y iresolution))))
	 (h (* height (+ (y uvn) (* (/ .5 h-ratio) (- 1.0 h-ratio))) h-ratio)))
    (v! (* w (- (x uvn) (* .25 (- 1.0 (/ width w))))) h)))

(define-function-library xrot ((tt :float))
  (m! 1.0 0.0 0.0
      0.0 (cos tt) (- (sin tt))
      0.0 (sin tt) (cos tt)))

(define-function-library yrot ((tt :float))
  (m! (cos tt) 0.0 (- (sin tt))
      0.0 1.0 0.0
      (sin tt) 0.0 (cos tt)))

(define-function-library zrot ((tt :float))
  (m! (cos tt) (- (sin tt)) 0.0
      (sin tt) (cos tt) 0.0
      0.0 0.0 1.0))

(defmacro build-light (name sdf eps)
  `(gfx:defun-g ,name ((a :vec3) (d :vec3) (s :vec3) (alpha :float) (p :vec3) (c :vec3)
		       (light-pos :vec3))
     (let* ((col a))
       (let* ((dx (- (,sdf p) (,sdf (- p (v! ,eps .0 .0)))))
	      (dy (- (,sdf p) (,sdf (- p (v! 0.0 ,eps 0.0)))))
	      (dz (- (,sdf p) (,sdf (- p (v! 0.0 0.0 ,eps)))))
	      (get-n (normalize (v! dx dy dz))))
	 (let* ((l1 light-pos)
		(lighting (vec3 0.0))
		(n (normalize get-n))
		(l (normalize (- l1 p)))
		(c (normalize (- c p)))
		(h (normalize (+ l c)))
		(ndl (dot n l))
		(ndh (dot n h)))
	   (cond! ((< ndl 0.0) (setf lighting (vec3 0.0)))
		  ((< ndh 0.0) (setf lighting (* d ndl)))
		  (t (setf lighting (+ (* d ndl) (* s (pow ndh alpha))))))
	   (incf col lighting)
	   col)))))

(define-macro-library texture! (texture uv &optional flip)
  (let* ((target (glsl::compile-form texture))
	 (size (ecase (glsl::code-type target)
		 (:sampler-2d 1.0)
		 (:sampler-2d-rect `(texture-size ,texture)))))
    (if flip
	(once-only (uv)
	  `(texture ,texture (* (v! (x ,uv) (- 1.0 (y ,uv))) ,size)))
      `(texture ,texture (* ,uv ,size)))))

(define-macro-library texture-rect (texture uv &optional flip)
  (let* ((size `(texture-size ,texture)))
    (if flip
	(once-only (uv)
	  `(texture ,texture (* (v! (x ,uv) (- 1.0 (y ,uv))) ,size)))
      `(texture ,texture (* ,uv ,size)))))

(define-function-library post-bloom ((texture :sampler-2d-rect) (size :float) (sigma :float) (horizon :int)
				     (uv :vec2) (resolution :vec2))
  (let* ((num-blur (/ size 1.0))
	 (blur-vec (v! 1.0 .0))
	 (incr (vec3 0.0))
	 (arg-value (v! 0.0 0.0 0.0 0.0))
	 (coefficient 0.0)
	 (texoffset (/ 1.0 (xy resolution))))
    (if (= horizon 0) (setf blur-vec (v! .0 1.0)))
    (setf (x incr) (/ 1.0 (* (sqrt (lisp (* 2.0 pi))) sigma))
	  (y incr) (exp (/ -0.5 (* sigma sigma)))
	  (z incr)  (* (y incr) (y incr)))
    (loop for i from 1.0 to num-blur 
	  do (incf arg-value
		   (* (texture texture
			       (* (texture-size texture)
				  (- uv (* i 1.2 texoffset blur-vec))))
		      (x incr)))
	     (incf arg-value
		   (* (texture texture
			       (* (texture-size texture)
				  (+ uv (* i 1.2 texoffset blur-vec))))
		      (x incr)))
	     (incf coefficient (* 1.0 (x incr)))
	     (setf (xy incr) (* (xy incr) (yz incr))))
    (/ arg-value coefficient)))

(define-function-library post-bloom2 ((texture :sampler-2d-rect) (uv :vec2) (resolution :vec2)
				      (power :float) (threshold :float) (range :float))
  (let* ((num-samples 1.0)
	 (color (texture texture (* (texture-size texture) uv)))
	 (depth 6.0))
    (loop for x from (- depth) to depth
	  do (loop for y from (- depth) to depth
		   do (let* ((add-color (texture texture
						 (* (texture-size texture)
						    (+ uv (* (v! x y) (/ (vec2 range) (xy resolution))))))))
			(when (> (max (x add-color) (max (y add-color) (z add-color))) threshold)
			  (let* ((dist (+ (length (v! x y)) 1.0))
				 (glow-color (max (/ (* add-color 128.0) (pow dist 2.0))
						  (v! 0.0 0.0 0.0 0.0))))
			    (when (> (max (x glow-color) (max (y glow-color) (z glow-color))) 0.0)
			      (incf color glow-color)
			      (setf num-samples (+ num-samples power))))))))
    (/ color num-samples)))

(define-function-library rand11 ((a :float))
  (fract (* (sin a) 10403.9)))

(define-function-library rand12 ((f :float))
  (fract (* (cos f) (v! 10003.579 37049.7))))

(define-function-library rand21 ((uv :vec2))
  (let* ((f (+ (x uv) (* (y uv) 37.0))))
    (fract (* (sin f) 104003.9))))

(define-function-library rand22 ((uv :vec2))
  (let* ((f (+ (x uv) (* (y uv) 37.0))))
    (fract (* (cos f) (v! 10003.579 37049.7)))))

(define-macro-library nearest-voxel (pos rd dist &optional (voxel-pad .2))
  `(let* ((dx (- (fract (x ,pos))))
	  (dz (- (fract (z ,pos))))
	  (voxel-pad ,voxel-pad))
     (when (> (x ,rd) 0.0) (setf dx (fract (- (x ,pos)))))
     (when (> (z ,rd) 0.0) (setf dz (fract (- (z ,pos)))))
     (let* ((nearest-voxel (+ (min (fract (/ dx (x ,rd)))
				   (fract (/ dz (z ,rd))))
			      voxel-pad)))
       (setf nearest-voxel (max voxel-pad nearest-voxel)
	     ,dist (min ,dist nearest-voxel)))))

(gfx:clear-pipeline)

