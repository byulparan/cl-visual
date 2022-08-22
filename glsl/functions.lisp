(in-package #:glsl)

(defun code-type-equal (&rest lst)
  (let* ((type (code-type (car lst))))
    (loop for obj in (cdr lst)
	  do (unless (eql type (code-type obj))
	       (return nil))
	  finally (return t))))

(defmacro add-function (name code-line (&key (read-p t) (write-p nil)) &body body)
  `(let ((,name (lambda ,(mapcar #'first (caar body))
  		    (cond ,@(loop for typespec in body
				  collect `((and ,@(mapcar (lambda (arg) (if (keywordp (second arg))
									`(eql (code-type ,(first arg)) ,(second arg))
								      `(funcall ',(second arg) ,(first arg))))
							  (car typespec)))
					    ,(when (third typespec)
					       `(assert ,(third typespec)))
					    (make-code-object ,(second typespec)
							      (format nil ,code-line
								      ,@(mapcar #'(lambda (a) `(code-line ,(first a))) (caar body)))
							     :read-p ,read-p
							     :write-p ,write-p)))
			  (t (error "not found function ~a with type ~a" ',name ',(mapcar #'second (caar body))))))))
     (setf (gethash ,(make-keyword name) *function-table*) ,name)))


(setf (gethash :+ *function-table*) '%+
      (gethash :- *function-table*) '%-
      (gethash :* *function-table*) '%*
      (gethash :/ *function-table*) '%/
      (gethash :atan *function-table*) '%atan
      (gethash :not *function-table*) '%not
      (gethash :break *function-table*) (lambda () (make-code-object :void "break"
								     :expression-p nil))
      (gethash :continue *function-table*) (lambda () (make-code-object :void "continue"
									:expression-p nil))
      (gethash :v! *function-table*) 'v!
      (gethash :ivec! *function-table*) 'ivec!
      (gethash :vec2 *function-table*) (lambda (v) (assert (eql :float (code-type v)))
					 (make-code-object :vec2 (format nil "vec2(~a)" (code-line v))))
      (gethash :vec3 *function-table*) (lambda (v) (assert (eql :float (code-type v)))
					 (make-code-object :vec3 (format nil "vec3(~a)" (code-line v))))
      (gethash :vec4 *function-table*) (lambda (v) (assert (eql :float (code-type v)))
					 (make-code-object :vec4 (format nil "vec4(~a)" (code-line v))))
      ;; mat3 function used for type-casting between matrinx
      (gethash :mat3 *function-table*) (lambda (v) (assert (matrix-p v))
					 (make-code-object :mat3 (format nil "mat3(~a)" (code-line v))))
      (gethash :m! *function-table*) 'm!
      (gethash :aref *function-table*) '%aref)

;;; vector
(defun v! (&rest rest)
  (let* ((len (apply #'+ (mapcar #'code-len rest))))
    (labels ((make-vec (type)
	       (make-code-object type (format nil "~a(~{~a~^,~})"
					      (string-downcase type)
					      (mapcar #'code-line rest)))))
      (cond ((every (lambda (a) (or (vector-p a) (ivector-p a) (number-p a))) rest)
	     (make-vec (ecase len (2 :vec2) (3 :vec3) (4 :vec4))))
	    (t (error "You can't make vector with ~a" rest))))))

(defun ivec! (&rest rest)
  (let* ((len (apply #'+ (mapcar #'code-len rest))))
    (labels ((make-vec (type)
	       (make-code-object type (format nil "~a(~{~a~^,~})"
					      (string-downcase type)
					      (mapcar #'code-line rest)))))
      (cond ((every (lambda (a) (or (vector-p a) (ivector-p a) (number-p a))) rest)
	     (make-vec (ecase len (2 :ivec2) (3 :ivec3) (4 :ivec4))))
	    (t (error "You can't make vector with ~a" rest))))))



(defun m! (&rest rest)
  (let* ((len (apply #'+ (mapcar #'code-len rest))))
    (labels ((make-mat (type)
	       (make-code-object type (format nil "~a(~{~a~^,~})"
					      (string-downcase type)
					      (mapcar #'code-line rest)))))
      (make-mat (ecase len (4 :mat2) (9 :mat3) (16 :mat4))))))


(defun %aref (object n)
  (assert (eql :int (code-type n)) nil "invalid aref access to ~s by ~a" (code-line object) (code-line n))
  (let ((indx (code-line n)))
    (when (numberp indx)
      (assert (> (code-len object) indx) nil "array index out of range '~a'" indx)))
  (make-code-object (cond ((vector-p object) :float)
			  ((matrix-p object) (ecase (code-len object)
					       (16 :vec4)
					       (9 :vec3)
					       (4 :vec2)))
			  ((typep object 'code-object-array) (base-type object))
			  (t (error "not array or vector in aref ~a" (code-line object))))
		    (format nil "~a[~a]" (code-line object) (code-line n))
		    :write-p t))

(add-function float "float(~a)" ()
  (((a number-p)) :float))

(add-function int "int(~a)" ()
  (((a number-p)) :int))

(add-function x "~a.x" (:write-p t)
  (((a vector-p)) :float))

(add-function y "~a.y" (:write-p t)
  (((a vector-p)) :float))

(add-function z "~a.z" (:write-p t)
  (((a :vec3)) :float)
  (((a :vec4)) :float))

(add-function w "~a.w" (:write-p t)
  (((a :vec4)) :float))

(add-function xx "~a.xx" (:write-p t)
  (((a vector-p)) :vec2))

(add-function xy "~a.xy" (:write-p t)
  (((a vector-p)) :vec2))

(add-function xz "~a.xz" (:write-p t)
  (((a :vec3)) :vec2)
  (((a :vec4)) :vec2))

(add-function xw "~a.xw" (:write-p t)
  (((a :vec4)) :vec2))

(add-function yy "~a.yy" (:write-p t)
  (((a vector-p)) :vec2))

(add-function yx "~a.yx" (:write-p t)
  (((a vector-p)) :vec2))

(add-function yz "~a.yz" (:write-p t)
  (((a :vec3)) :vec2)
  (((a :vec4)) :vec2))

(add-function yw "~a.yw" (:write-p t)
  (((a :vec4)) :vec2))

(add-function zz "~a.zz" (:write-p t)
  (((a :vec3)) :vec2)
  (((a :vec4)) :vec2))

(add-function zx "~a.zx" (:write-p t)
  (((a :vec3)) :vec2)
  (((a :vec4)) :vec2))

(add-function zy "~a.zy" (:write-p t)
  (((a :vec3)) :vec2)
  (((a :vec4)) :vec2))

(add-function zw "~a.zw" (:write-p t)
  (((a :vec4)) :vec2))

(add-function ww "~a.ww" (:write-p t)
  (((a :vec4)) :vec2))

(add-function wx "~a.wx" (:write-p t)
  (((a :vec4)) :vec2))

(add-function wy "~a.wy" (:write-p t)
  (((a :vec4)) :vec2))

(add-function wz "~a.wz" (:write-p t)
  (((a :vec4)) :vec2))

(add-function xyz "~a.xyz" (:write-p t)
  (((a :vec3)) :vec3)
  (((a :vec4)) :vec3))

(add-function zyx "~a.zyx" (:write-p t)
  (((a :vec3)) :vec3)
  (((a :vec4)) :vec3))

;;; 
(add-function add "~a + ~a" nil
  (((a :int) (b :int)) :int)
  (((a :float) (b :float)) :float)
  (((a :float) (b :int)) :float)
  (((a :int) (b :float)) :float)
  (((a vector-p) (b number-p)) (code-type a))
  (((a number-p) (b vector-p)) (code-type b))
  (((a vector-p) (b vector-p)) (code-type a) (code-type-equal a b)))

(defun %+ (&rest args)
  (let ((object (reduce (gethash :add *function-table*) args)))
    (with-slots (code-line) object
      (setf code-line (format nil "(~a)" code-line)))
    object))

(add-function minus "~a - ~a" nil
  (((a :int) (b :int)) :int)
  (((a :float) (b :float)) :float)
  (((a :float) (b :int)) :float)
  (((a :int) (b :float)) :float)
  (((a vector-p) (b number-p)) (code-type a))
  (((a number-p) (b vector-p)) (code-type b))
  (((a vector-p) (b vector-p)) (code-type a) (code-type-equal a b)))

(defun %- (&rest args)
  (if (= (length args) 1) (make-code-object (code-type (car args)) (format nil "(- ~a)" (code-line (car args))))
    (let ((object (reduce (gethash :minus *function-table*) args)))
      (with-slots (code-line) object
	(setf code-line (format nil "(~a)" code-line)))
      object)))

(add-function multiply "~a * ~a" nil
  (((a :int) (b :int)) :int)
  (((a :float) (b :float)) :float)
  (((a :float) (b :int)) :float)
  (((a :int) (b :float)) :float)
  (((a vector-p) (b number-p)) (code-type a))
  (((a number-p) (b vector-p)) (code-type b))
  (((a vector-p) (b vector-p)) (code-type a) (code-type-equal a b))
  (((a matrix-p) (b vector-p)) (code-type b) (= (sqrt (code-len a)) (code-len b)))
  (((a vector-p) (b matrix-p)) (code-type a) (= (sqrt (code-len b)) (code-len a)))
  (((a matrix-p) (b matrix-p)) (code-type a) (= (code-len a) (code-len b))))

(defun %* (&rest args)
  (let ((object (reduce (gethash :multiply *function-table*) args)))
    (with-slots (code-line) object
      (setf code-line (format nil "(~a)" code-line)))
    object))

(add-function divide "~a / ~a" nil
  (((a :int) (b :int)) :int)
  (((a :float) (b :float)) :float)
  (((a :float) (b :int)) :float)
  (((a :int) (b :float)) :float)
  (((a vector-p) (b number-p)) (code-type a))
  (((a number-p) (b vector-p)) (code-type b))
  (((a vector-p) (b vector-p)) (code-type a) (code-type-equal a b)))

(defun %/ (&rest args)
  (let ((object (reduce (gethash :divide *function-table*) args)))
    (with-slots (code-line) object
      (setf code-line (format nil "(~a)" code-line)))
    object))

(add-function %_atan "atan(~a)" nil
  (((a number-p))  :float)
  (((a vector-p)) (code-type a)))

(add-function %_atan2 "atan(~a,~a)" nil
  (((a number-p) (b number-p))  :float)
  (((a vector-p) (b vector-p)) (code-type a) (code-type-equal a b)))

(defun %atan (&rest args)
  (ecase (length args)
    (1 (apply (gethash :%_atan *function-table*) args))
    (2 (apply (gethash :%_atan2 *function-table*) args))))

(defun %not (bool)
  (ecase (code-type bool)
    ((:bool :bvec2 :bvec3 :bvec4) (make-code-object (code-type bool) (format nil "!(~a)" (code-line bool))))))


;;; 
(add-function logand "(~a & ~a)" nil
  (((a :int) (b :int)) :int)
  (((a ivector-p) (b :int)) (code-type a))
  (((a :int) (b ivector-p)) (code-type b))
  (((a ivector-p) (b ivector-p)) (code-type a) (code-type-equal a b)))

(add-function logior "(~a | ~a)" nil
  (((a :int) (b :int)) :int)
  (((a ivector-p) (b :int)) (code-type a))
  (((a :int) (b ivector-p)) (code-type b))
  (((a ivector-p) (b ivector-p)) (code-type a) (code-type-equal a b)))

(add-function >> "(~a >> ~a)" nil
  (((a :int) (b :int)) :int)
  (((a ivector-p) (b :int)) (code-type a))
  (((a :int) (b ivector-p)) (code-type b))
  (((a ivector-p) (b ivector-p)) (code-type a) (code-type-equal a b)))

(add-function << "(~a << ~a)" nil
  (((a :int) (b :int)) :int)
  (((a ivector-p) (b :int)) (code-type a))
  (((a :int) (b ivector-p)) (code-type b))
  (((a ivector-p) (b ivector-p)) (code-type a) (code-type-equal a b)))


;;; 
(add-function = "(~a == ~a)" nil
  (((a number-p) (b number-p)) :bool))

(add-function /= "(~a != ~a)" nil
  (((a number-p) (b number-p)) :bool))

(add-function > "(~a > ~a)" nil
  (((a number-p) (b number-p)) :bool))

(add-function >= "(~a >= ~a)" nil
  (((a number-p) (b number-p)) :bool))

(add-function < "(~a < ~a)" nil
  (((a number-p) (b number-p)) :bool))

(add-function <= "(~a <= ~a)" nil
  (((a number-p) (b number-p)) :bool))

;;;
(add-function abs "abs(~a)" nil
  (((a :int)) :int)
  (((a :float)) :float)
  (((a vector-p)) (code-type a)))

(add-function acos "acos(~a)" nil
  (((a number-p))  :float)
  (((a vector-p)) (code-type a)))

(add-function asin "asin(~a)" nil
  (((a number-p))  :float)
  (((a vector-p)) (code-type a)))

(add-function ceil "ceil(~a)" nil
  (((a number-p))  :float)
  (((a vector-p)) (code-type a)))

(add-function clamp "clamp(~a,~a,~a)" nil
  (((a number-p) (b number-p) (c number-p)) :float)
  (((a vector-p) (b number-p) (c number-p)) (code-type a))
  (((a vector-p) (b vector-p) (c vector-p)) (code-type a) (code-type-equal a b c)))

(add-function cos "cos(~a)" nil
  (((a number-p))  :float)
  (((a vector-p)) (code-type a)))

(add-function cross "cross(~a,~a)" nil
  (((a :vec3) (b :vec3)) :vec3))

(add-function dfdx "dFdx(~a)" nil
  (((a number-p)) :float)
  (((a vector-p)) (code-type a)))

(add-function dfdy "dFdy(~a)" nil
  (((a number-p)) :float)
  (((a vector-p)) (code-type a)))

(add-function fwidth "fwidth(~a)" nil
  (((a number-p)) :float)
  (((a vector-p)) (code-type a)))

(add-function degrees "degrees(~a)" nil
  (((a number-p)) :float)
  (((a vector-p)) (code-type a)))

(add-function distance "distance(~a,~a)" nil
  (((a number-p) (b number-p)) :float)
  (((a vector-p) (b vector-p)) :float (code-type-equal a b)))

(add-function dot "dot(~a,~a)" nil
  (((a number-p) (b number-p)) :float)
  (((a vector-p) (b vector-p)) :float (code-type-equal a b)))

(add-function exp "exp(~a)" nil
  (((a number-p)) :float)
  (((a vector-p)) (code-type a)))

(add-function exp2 "exp2(~a)" nil
  (((a number-p)) :float)
  (((a vector-p)) (code-type a)))

(add-function faceforward "faceforward(~a,~a,~a)" nil
  (((a number-p) (b number-p) (c number-p)) :float)
  (((a vector-p) (b vector-p) (c vector-p)) (code-type a) (code-type-equal a b c)))

(add-function floor "floor(~a)" nil
  (((a number-p)) :float)
  (((a vector-p)) (code-type a)))

(add-function fract "fract(~a)" nil
  (((a number-p)) :float)
  (((a vector-p)) (code-type a)))

(add-function inversesqrt "inversesqrt(~a)" nil
  (((a number-p)) :float)
  (((a vector-p)) (code-type a)))

(add-function length "length(~a)" nil
  (((a number-p)) :float)
  (((a vector-p)) :float))

(add-function log "log(~a)" nil
  (((a number-p)) :float)
  (((a vector-p)) (code-type a)))

(add-function log2 "log2(~a)" nil
  (((a number-p)) :float)
  (((a vector-p)) (code-type a)))

(add-function min "min(~a,~a)" nil
  (((a number-p) (b number-p)) :float)
  (((a vector-p) (b number-p)) (code-type a))
  (((a vector-p) (b vector-p)) (code-type a) (code-type-equal a b)))

(add-function mix "mix(~a,~a,~a)" nil
  (((a number-p) (b number-p) (c number-p)) :float)
  (((a vector-p) (b vector-p) (c number-p)) (code-type a) (code-type-equal a b))
  (((a vector-p) (b vector-p) (c vector-p)) (code-type a) (code-type-equal a b c)))

(add-function max "max(~a,~a)" nil
  (((a number-p) (b number-p)) :float)
  (((a vector-p) (b number-p)) (code-type a))
  (((a vector-p) (b vector-p)) (code-type a) (code-type-equal a b)))

(add-function mod "mod(~a,~a)" nil
  (((a number-p) (b number-p)) :float)
  (((a vector-p) (b number-p)) (code-type a))
  (((a vector-p) (b vector-p)) (code-type a) (code-type-equal a b)))

(add-function normalize "normalize(~a)" nil
  (((a number-p)) :float)
  (((a vector-p)) (code-type a)))

(add-function pow "pow(~a,~a)" nil
  (((a number-p) (b number-p)) :float)
  (((a vector-p) (b vector-p)) (code-type a) (code-type-equal a b)))

(add-function radians "radians(~a)" nil
  (((a number-p)) :float)
  (((a vector-p)) (code-type a)))

(add-function refract "refract(~a,~a,~a)" nil
  (((a number-p) (b number-p) (c number-p)) :float)
  (((a vector-p) (b vector-p) (c number-p)) (code-type a) (code-type-equal a b)))

(add-function reflect "reflect(~a,~a)" nil
  (((a number-p) (b number-p)) :float)
  (((a vector-p) (b vector-p)) (code-type a) (code-type-equal a b)))

(add-function sign "sign(~a)" nil
  (((a number-p))  :float)
  (((a vector-p)) (code-type a)))

(add-function sin "sin(~a)" nil
  (((a number-p))  :float)
  (((a vector-p)) (code-type a)))

(add-function step "step(~a,~a)" nil
  (((a number-p) (b number-p))  :float)
  (((a number-p) (b vector-p)) (code-type b))
  (((a vector-p) (b vector-p)) (code-type a) (code-type-equal a b)))

(add-function smoothstep "smoothstep(~a,~a,~a)" nil
  (((a number-p) (b number-p) (c number-p)) :float)
  (((a number-p) (b number-p) (c vector-p)) (code-type c))
  (((a vector-p) (b vector-p) (c vector-p)) (code-type a) (code-type-equal a b c)))

(add-function sqrt "sqrt(~a)" nil
  (((a number-p))  :float)
  (((a vector-p)) (code-type a)))

(add-function tan "tan(~a)" nil
  (((a number-p))  :float)
  (((a vector-p)) (code-type a)))

(add-function transpose "transpose(~a)" nil
  (((a matrix-p)) (code-type a)))

(add-function inverse "inverse(~a)" nil
  (((a matrix-p)) (code-type a)))

;;; bool
(defun to-bvec (vec)
  (ecase (code-type vec)
    (:vec2 :bvec2)
    (:vec3 :bvec3)
    (:vec4 :bvec4)))

(add-function less-than "lessThan(~a,~a)" nil
  (((a vector-p) (b vector-p)) (to-bvec a) (code-type-equal a b)))

(add-function less-than-equal "lessThanEqual(~a,~a)" nil
  (((a vector-p) (b vector-p)) (to-bvec a) (code-type-equal a b)))

(add-function greater-than "greaterThan(~a,~a)" nil
  (((a vector-p) (b vector-p)) (to-bvec a) (code-type-equal a b)))

(add-function greater-than-equal "greaterThanEqual(~a,~a)" nil
  (((a vector-p) (b vector-p)) (to-bvec a) (code-type-equal a b)))

(add-function equal "equal(~a,~a)" nil
  (((a vector-p) (b vector-p)) (to-bvec a) (code-type-equal a b)))

(add-function not-equal "notEqual(~a,~a)" nil
  (((a vector-p) (b vector-p)) (to-bvec a) (code-type-equal a b)))

(add-function any "any(~a)" nil
  (((a bvector-p)) :bool))

(add-function all "all(~a)" nil
  (((a bvector-p)) :bool))




(defun %discard ()
  (make-code-object :void (format nil "discard") :expression-p nil))

(setf (gethash :discard *function-table*) #'%discard)

(defun %texture (a b &optional bias)
  ;; (assert (or (and (member (code-type a) '(:sampler-2d :sampler-2d-rect))  (eql (code-type b) :vec2))
  ;; 	      (and (eql (code-type a) :sampler-cube) (eql (code-type b) :vec3))))
  (when bias (assert (eql (code-type bias) :float)))
  (let* ((fun-name (cond ((= *glsl-version* 330) "texture")
			 ((and (= *glsl-version* 120) (eql (code-type a) :sampler-2d)) "texture2D")
			 ((and (= *glsl-version* 120) (eql (code-type a) :sampler-2d-rect)) "texture2DRect")
			 (t (error "invalid version %texture")))))
    (make-code-object :vec4 (format nil "~a(~a,~a~@[,~a~])" fun-name (code-line a) (code-line b) (when bias (code-line bias)))
		      :write-p nil)))

(setf (gethash :texture *function-table*) #'%texture)

(defun %texture-lod (a b lod)
  (assert (and (member (code-type a) '(:sampler-2d :sampler-2d-rect))
	       (eql (code-type b) :vec2)
	       (eql (code-type lod) :float)))
  (make-code-object :vec4 (format nil "~a(~a,~a,~a)" "textureLod" (code-line a) (code-line b) (code-line lod))
		    :write-p nil))

(setf (gethash :texture-lod *function-table*) #'%texture-lod)

(add-function texel-fetch "texelFetch(~a,~a,~a)" nil
  (((a :sampler-2d) (b :ivec2) (c :int)) :vec4))

(defun %texture-size (sampler)
  (make-code-object :vec2 (format nil "textureSize(~a)" (code-line sampler))))

(setf (gethash :texture-size *function-table*) #'%texture-size)

(defun emit-vertex ()
  (make-code-object :void "EmitVertex()"))

(setf (gethash :emit-vertex *function-table*) #'emit-vertex)

(defun end-primitive ()
  (make-code-object :void "EndPrimitive()"))

(setf (gethash :end-primitive *function-table*) #'end-primitive)

(defun gl-in-position (index)
  (make-code-object :vec4 (format nil "gl_in[~d].gl_Position" (code-line index))))

(setf (gethash :gl-in-position *function-table*) #'gl-in-position)
