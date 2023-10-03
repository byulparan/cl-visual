(defpackage :shader-lib
  (:shadowing-import-from #:gfx #:defpipeline :fill)
  (:nicknames :sl)
  (:use #:cl :glsl :cl-visual)
  
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

(define-function-library syphon-ratio ((uvn :vec2) (iresolution :vec2) (width :float) (height :float) (x-ratio :float) (h-ratio :float))
  (let* ((w (* (x iresolution) x-ratio (/ height (y iresolution))))
	 (h (* height (+ (y uvn) (* (/ .5 h-ratio) (- 1.0 h-ratio))) h-ratio)))
    (v! (* w (- (x uvn) (* .5 (- 1.0 (/ width w))))) h)))

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


(define-function-library rotate-2d ((r :float))
  (m! (cos r) (sin r) (- (sin r)) (cos r)))

(define-function-library rotate-3d ((angle :float) (axis :vec3))
  (let* ((a (normalize axis))
	 (s (sin angle))
	 (c (cos angle))
	 (r (- 1.0 c)))
    (m! (+ (* (x a) (x a) r) c)
	(+ (* (y a) (x a) r) (* (z a) s))
	(- (* (z a) (x a) r) (* (y a) s))
	(- (* (x a) (y a) r) (* (z a) s))
	(+ (* (y a) (y a) r) c)
	(+ (* (z a) (y a) r) (* (x a) s))
	(+ (* (x a) (z a) r) (* (y a) s))
	(- (* (y a) (z a) r) (* (x a) s))
	(+ (* (z a) (z a) r) c))))


(define-function-library translate ((vec :vec3))
  (transpose
   (m! 1.0 0.0 0.0 (x vec)
       0.0 1.0 0.0 (y vec)
       0.0 0.0 1.0 (z vec)
       0.0 0.0 0.0 1.0)))

(define-function-library rotate ((angle :float) (vec :vec3))
  (let* ((rot (rotate-3d angle vec)))
    (m! (aref rot 0) 0.0
	(aref rot 1) 0.0
	(aref rot 2) 0.0
	(vec3 0.0) 1.0)))

(define-function-library scale ((vec :vec3))
  (m! (x vec) 0.0 0.0 0.0
      0.0 (y vec) 0.0 0.0
      0.0 0.0 (z vec) 0.0
      0.0 0.0 0.0 1.0))




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
	(alexandria:once-only (uv)
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





;; ================================================================================
;; twiGL
;; ================================================================================

(define-function-library mod289-fl ((x :float))
  (- x (* (floor (* x (/ 1.0 289.0))) 289.0)))

(define-function-library mod289-v2 ((x :vec2))
  (- x (* (floor (* x (/ 1.0 289.0))) 289.0)))

(define-function-library mod289-v3 ((x :vec3))
  (- x (* (floor (* x (/ 1.0 289.0))) 289.0)))

(define-function-library mod289-v4 ((x :vec4))
  (- x (* (floor (* x (/ 1.0 289.0))) 289.0)))

(define-macro-library mod289 (x)
  (let* ((type (glsl::code-type (glsl::compile-form  x))))
    (ecase type
      (:int `(mod289-fl (* 1.0 ,x)))
      (:float `(mod289-fl ,x))
      (:vec2  `(mod289-v2 ,x))
      (:vec3  `(mod289-v3 ,x))
      (:vec4  `(mod289-v4 ,x)))))

(unexport '(mod289-fl mod289-v2 mod289-v3 mod289-v4))



(define-function-library permute-fl ((x :float))
  (mod289-fl (* (+ (* x 34.0) 1.0) x)))

(define-function-library permute-v3 ((x :vec3))
  (mod289-v3 (* (+ (* x 34.0) 1.0) x)))

(define-function-library permute-v4 ((x :vec4))
  (mod289-v4 (* (+ (* x 34.0) 1.0) x)))

(define-macro-library permute (x)
  (let* ((type (glsl::code-type (glsl::compile-form  x))))
    (ecase type
      (:int `(permute-fl (* 1.0 ,x)))
      (:float `(permute-fl ,x))
      (:vec3  `(permute-v3 ,x))
      (:vec4  `(permute-v4 ,x)))))

(unexport '(permute-fl permute-v3 permute-v4))



(define-function-library taylor-inv-sqrt-fl ((r :float))
  (- 1.79284291400159d0 (* 0.85373472095314d0 r)))

(define-function-library taylor-inv-sqrt-v4 ((r :vec4))
  (- 1.79284291400159d0 (* 0.85373472095314d0 r)))

(define-macro-library taylor-inv-sqrt (x)
  (let* ((type (glsl::code-type (glsl::compile-form  x))))
    (ecase type
      (:int `(taylor-inv-sqrt-fl (* 1.0 ,x)))
      (:float `(taylor-inv-sqrt-fl ,x))
      (:vec4  `(taylor-inv-sqrt-v4 ,x)))))

(unexport '(taylor-inv-sqrt-fl taylor-inv-sqrt-v4))



(define-function-library snoise-2d ((v :vec2))
  (let* ((c (v! 0.211324865405187d0
		0.366025403784439d0
		-0.577350269189626d0
		0.024390243902439d0))
	 (i (floor (+ v (dot v (yy c)))))
	 (x0 (+ (- v i) (dot i (xx c))))
	 (i1 (if (> (x x0) (y x0)) (v! 1.0 0.0) (v! 0.0 1.0)))
	 (x12 (+ (s~ x0 xyxy) (s~ c xxzz))))
    (setf (xy x12) (- (xy x12) i1))
    (setf i (mod289-v2 i))
    (let* ((p (permute-v3 (+ (permute-v3 (+ (y i) (v! 0.0 (y i1) 1.0))) (x i) (v! 0.0 (x i1) 1.0))))
	   (m (max (- 0.5 (v! (dot x0 x0) (dot (xy x12) (xy x12)) (dot (s~ x12 zw) (s~ x12 zw)))) 0.0)))
      (setf m (* m m))
      (setf m (* m m))
      (let* ((x (- (* 2.0 (fract (* p (s~ c www)))) 1.0))
	     (h (- (abs x) 0.5))
	     (ox (floor (+ x 0.5)))
	     (a0 (- x ox)))
	(multf m (- 1.79284291400159d0 (* 0.85373472095314d0 (+ (* a0 a0) (* h h)))))
	(let* ((g (vec3 0.0)))
	  (setf (x g) (+ (* (x a0) (x x0)) (* (x h) (y x0))))
	  (setf (yz g) (+ (* (yz a0) (xz x12)) (* (yz h) (s~ x12 yw))))
	  (* 130.0 (dot m g)))))))

(define-function-library snoise-3d ((v :vec3))
  (let* ((c (v! (/ 1.0 6.0) (/ 1.0 3.0)))
	 (d (v! 0.0 0.5 1.0 2.0))
	 (i (floor (+ v (dot v (s~ c yyy)))))
	 (x0 (+ (- v i) (dot i (s~ c xxx))))
	 (g (step (s~ x0 yzx) (s~ x0 xyz)))
	 (l (- 1.0 g))
	 (i1 (min (s~ g xyz) (s~ l zxy)))
	 (i2 (max (s~ g xyz) (s~ l zxy)))
	 (x1 (+ (- x0 i1) (s~ c xxx)))
	 (x2 (+ (- x0 i2) (s~ c yyy)))
	 (x3 (- x0 (s~ d yyy))))
    (setf i (mod289-v3 i))
    (let* ((p (permute-v4 (+ (permute-v4 (+ (permute-v4 (+ (z i) (v! 0.0 (z i1) (z i2) 1.0)))
				      (y i) (v! 0.0 (y i1) (y i2) 1.0)))
			  (x i) (v! 0.0 (x i1) (x i2) 1.0))))
	   (n_ 0.142857142857d0)
	   (ns (- (* n_ (s~ d wyz)) (s~ d xzx)))
	   (j (- p (* 49.0 (floor (* p (z ns) (z ns))))))
	   (x_ (floor (* j (z ns))))
	   (y_ (floor (- j (* 7.0 x_))))
	   (x (+ (* x_ (x ns)) (s~ ns yyyy)))
	   (y (+ (* y_ (x ns)) (s~ ns yyyy)))
	   (h (- 1.0 (abs x) (abs y)))
	   (b0 (v! (xy x) (xy y)))
	   (b1 (v! (s~ x zw) (s~ y zw)))
	   (s0 (+ (* (floor b0) 2.0) 1.0))
	   (s1 (+ (* (floor b1) 2.0) 1.0))
	   (sh (- (step h (vec4 0.0))))
	   (a0 (+ (s~ b0 xzyw) (* (s~ s0 xzyw) (s~ sh xxyy))))
	   (a1 (+ (s~ b1 xzyw) (* (s~ s1 xzyw) (s~ sh zzww))))
	   (p0 (v! (xy a0) (x h)))
	   (p1 (v! (zw a0) (y h)))
	   (p2 (v! (xy a1) (z h)))
	   (p3 (v! (zw a1) (w h)))
	   (norm (taylor-inv-sqrt-v4 (v! (dot p0 p0) (dot p1 p1) (dot p2 p2) (dot p3 p3)))))
      (multf p0 (x norm))
      (multf p1 (y norm))
      (multf p2 (z norm))
      (multf p3 (w norm))
      (let* ((m (max (- 0.6 (v! (dot x0 x0) (dot x1 x1) (dot x2 x2) (dot x3 x3))) 0.0)))
	(setf m (* m m))
	(* 42.0 (dot (* m m) (v! (dot p0 x0) (dot p1 x1) (dot p2 x2) (dot p3 x3))))))))


(define-function-library grad4 ((j :float) (ip :vec4))
  (let* ((ones (v! 1.0 1.0 1.0 -1.0))
	 (p (vec4 0.0))
	 (s (vec4 0.0)))
    (setf (xyz p) (- (* (floor (* (fract (* (vec3 j) (xyz ip))) 7.0)) (z ip)) 1.0))
    (setf (w p) (- 1.5 (dot (abs (s~ p xyz)) (s~ ones xyz))))
    (setf s (v! (less-than p (vec4 0.0))))
    (setf (xyz p) (+ (xyz p) (* (- (* (xyz s) 2.0) 1.0) (s~ s www))))
    p))


(define-function-library snoise-4d ((v :vec4))
  (let* ((c (v! 0.138196601125011d0
		0.276393202250021d0
		0.414589803375032d0
		-0.447213595499958d0))
	 (i (floor (+ v (dot v (vec4 0.309016994374947451d0)))))
	 (x0 (+ (- v i) (dot i (s~ c xxxx))))
	 (i0 (vec4 .0))
	 (is-x (step (s~ x0 yzw) (s~ x0 xxx)))
	 (is-yz (step (s~ x0 zww) (s~ x0 yyz))))
    (setf (x i0) (+ (x is-x) (y is-x) (z is-x)))
    (setf (s~ i0 yzw) (- 1.0 is-x))
    (incf (y i0) (+ (x is-yz) (y is-yz)))
    (incf (zw i0) (- 1.0 (xy is-yz)))
    (incf (z i0) (z is-yz))
    (incf (w i0) (- 1.0 (z is-yz)))
    (let* ((i3 (clamp i0 .0 1.0))
	   (i2 (clamp (- i0 1.0) 0.0 1.0))
	   (i1 (clamp (- i0 2.0) 0.0 1.0))
	   (x1 (+ (- x0 i1) (s~ c xxxx)))
	   (x2 (+ (- x0 i2) (s~ c yyyy)))
	   (x3 (+ (- x0 i3) (s~ c zzzz)))
	   (x4 (+ x0 (s~ c wwww))))
      (setf i (mod289-v4 i))
      (let* ((j0 (permute-fl (+ (permute-fl (+ (permute-fl (+ (permute-fl (w i)) (z i))) (y i))) (x i))))
	     (j1 (permute-v4 (+ (permute-v4 (+ (permute-v4 (+ (permute-v4 (+ (w i) (v! (w i1) (w i2) (w i3) 1.0)))
						     (z i) (v! (z i1) (z i2) (z i3) 1.0)))
					  (y i) (v! (y i1) (y i2) (y i3) 1.0)))
			      (x i) (v! (x i1) (x i2) (x i3) 1.0))))
	     (ip (v! (/ 1.0 294.0)  (/ 1.0 49.0) (/ 1.0 7.0) 0.0))
	     (p0 (grad4 j0 ip))
	     (p1 (grad4 (x j1) ip))
	     (p2 (grad4 (y j1) ip))
	     (p3 (grad4 (z j1) ip))
	     (p4 (grad4 (w j1) ip))
	     (norm (taylor-inv-sqrt-v4 (v! (dot p0 p0) (dot p1 p1) (dot p2 p2) (dot p3 p3)))))
	(multf p0 (x norm))
	(multf p1 (y norm))
	(multf p2 (z norm))
	(multf p3 (w norm))
	(multf p4 (taylor-inv-sqrt-fl (dot p4 p4)))
	(let* ((m0 (max (- 0.6 (v! (dot x0 x0) (dot x1 x1) (dot x2 x2))) 0.0))
	       (m1 (max (- 0.6 (v! (dot x3 x3) (dot x4 x4))) 0.0)))
	  (setf m0 (* m0 m0))
	  (setf m1 (* m1 m1))
	  (* 49.0
	     (+ (dot (* m0 m0) (v! (dot p0 x0) (dot p1 x1) (dot p2 x2)))
		(dot (* m1 m1) (v! (dot p3 x3) (dot p4 x4))))))))))


(define-function-library fsnoise ((c :vec2))
  (fract (* (sin (dot c (v! 12.9898d0 78.233d0))) 43758.5453d0)))

(define-function-library fsnoise-digits ((c :vec2))
  (fract (* (sin (dot c (v! 0.129898d0 0.78233d0))) 437.585453d0)))

(define-function-library hsv ((h :float) (s :float) (v :float))
  (let* ((t (v! 1.0 (/ 2.0 3.0) (/ 1.0 3.0) 3.0))
	 (p (abs (- (* (fract (+ (vec3 h) (xyz t))) 6.0) (vec3 (w t))))))
    (* v (mix (vec3 (x t)) (clamp (- p (vec3 (x t))) 0.0 1.0) s))))



(define-macro-library for-loop ((bindings limit accur) &body body)
  `(let ,(cdr bindings)
     (glsl::for ,(car bindings) ,limit ,accur
		,@body)))


(define-macro-library with-twigl (dummy &body body)
  (let* ((r (intern "R"))
	 (m (intern "M"))
	 (fc (intern "FC"))
	 (o (intern "O")))
    `(let* ((,r (xy iresolution))
	    (,m (/ (xy imouse) ,r))
	    (_fc (xy gl-frag-coord))
	    (,fc (v! _fc .5 1.0))
	    (s 0.0)
	    (t itime)
	    (,o (vec4 .0)))
       ,@body
       (v! (clamp (xyz ,o) 0.0 1.0) 1.0))))



;; ================================================================================


(gfx:clear-pipeline)

