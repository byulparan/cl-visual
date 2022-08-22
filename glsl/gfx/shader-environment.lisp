(in-package :gfx)

(defclass shader-environment ()
  ((shaders :initform nil
	    :accessor shaders)
   (buffers :initform nil
	    :accessor buffers)))


;;; gpu-stream
(defun register-gpu-stream (environment gpu-stream)
  "create VAO and VBO, IBO. and register vertex-attrib-pointer to VAO"
  (unless (getf (buffers environment) gpu-stream)
    (let ((core-profile (%gpu-stream-core-profile gpu-stream))
	  (vao nil)
	  (vbo nil)
	  (ibo nil))
      (when core-profile
	(setf vao (gl:gen-vertex-array)))
      (let* ((stream-type (%gpu-stream-types gpu-stream))
	     (sizes (mapcar #'type-size stream-type))
	     (offsets (list (cffi:null-pointer)))
	     (strides 0)
	     (float-size (cffi:foreign-type-size :float)))
	(setf strides (apply #'+ (mapcar #'(lambda (s)(* float-size s)) sizes))
	      offsets (cons 0 (loop for i in (mapcar #'(lambda (s)(* float-size s)) (butlast sizes))
				    and b = 0 then (+ b i)
				    collect (+ b i))))
	(when core-profile
	  (gl:bind-vertex-array vao))
	(setf vbo (gl:gen-buffer))
	(gl:bind-buffer :array-buffer vbo)
	(loop for i from 0 below (length sizes)
	      do (gl:enable-vertex-attrib-array i)
		 (when core-profile
		   (gl:vertex-attrib-pointer i (elt sizes i) :float nil strides (elt offsets i))))
	(when (%gpu-stream-index-array gpu-stream)
	  (setf ibo (gl:gen-buffer))
	  (gl:bind-buffer :element-array-buffer ibo))
	(setf (%gpu-stream-info gpu-stream) (list :sizes sizes :strides strides :offsets offsets))
	(when core-profile
	  (gl:bind-vertex-array 0)))
      (setf (getf (buffers environment) gpu-stream) (list -1 vao vbo ibo)))))

(defun update-gpu-stream (environment gpu-stream)
  "update contents of VBO."
  (register-gpu-stream environment gpu-stream)
  (when (/= (%gpu-stream-update-time gpu-stream)
	    (car (getf (buffers environment) gpu-stream)))
    (gl:bind-buffer :array-buffer (third (getf (buffers environment) gpu-stream)))
    (let ((gpu-array (%gpu-stream-array gpu-stream)))
      (cffi:with-pointer-to-vector-data (pointer gpu-array)
	(%gl:buffer-data :array-buffer (* (cffi:foreign-type-size :float) (length gpu-array))
			 pointer :static-draw)))
    (when (fourth (getf (buffers environment) gpu-stream))
      (gl:bind-buffer :element-array-buffer (fourth (getf (buffers environment) gpu-stream)))
      (gl:with-gl-array (array :unsigned-int :count (length (%gpu-stream-index-array gpu-stream)))
	(let ((vert (%gpu-stream-index-array gpu-stream)))
	  (loop for v in vert
		for i from 0
		do (setf (gl:glaref array i) v)))
	(gl:buffer-data :element-array-buffer :static-draw array)))
    (setf (car (getf (buffers environment) gpu-stream)) (%gpu-stream-update-time gpu-stream))))


;;; shader program
(defun make-shader-program (pipeline)
  (let ((success t)
	(program (%gl:create-program))
	(shaders nil))
    (handler-case (progn
		    (loop for src in (%pipeline-shader-src pipeline)
			  for type in '(:vertex-shader :geometry-shader :fragment-shader)
			  when src
			    do (let* ((shader (gl:create-shader type)))
				 (pushnew shader shaders)
				 (gl:shader-source shader src)
				 (gl:compile-shader shader)
				 (unless (gl:get-shader shader :compile-status)
				   (error (gl:get-shader-info-log shader)))
				 (gl:attach-shader program shader))))
      (error (c) (progn (break (format nil "~a" c))
			(dolist (sh shaders)
			  (gl:delete-shader sh))
			(gl:delete-program program)
			(setf success nil))))
    (if success (progn
		  (gl:link-program program)
		  (values program (reverse shaders)))
      (values nil nil))))

(defun update-pipeline (environment pipeline)
  "refresh opengl program and shaders used native gl compiler."
  (let* ((name (%pipeline-name pipeline))
	 (src (getf (shaders environment) name)))
    (when (or (not src)
	      (/= (%pipeline-update-time pipeline)
		  (car src)))
      (when src
	(let* ((program (second src))
	       (shaders (third src)))
	  (dolist (sh shaders)
	    (gl:detach-shader program sh)
	    (gl:delete-shader sh))
	  (gl:delete-program program)
	  (alexandria:remove-from-plistf (shaders environment) name)))
      (multiple-value-bind (program shaders)
	  (make-shader-program pipeline)
	(when program
	  (setf (getf (shaders environment) name) (list (%pipeline-update-time pipeline) program shaders)))))))

(defun release-environment (shader-environment)
  "Remove opengl program and shader and VAO, VBO, IBO resources on environment.
If you want reinitialize/release shader resources then call this.
this function can call repeatly."
  (loop for (nil shader) on (shaders shader-environment) by #'cddr
	do (destructuring-bind (update-time program shaders)
	       shader
	     (declare (ignore update-time))
	     (dolist (sh shaders)
	       (gl:detach-shader program sh)
	       (gl:delete-shader sh))
	     (gl:delete-program program)))
  (loop for (nil buffer) on (buffers shader-environment) by #'cddr
	do (destructuring-bind (update-time vao vbo ibo)
	       buffer
	     (declare (ignore update-time)) 
	     (gl:delete-buffers (list vbo))
	     (when ibo (gl:delete-buffers (list ibo)))
	     (when vao (gl:delete-vertex-arrays (list vao)))))
  (setf (shaders shader-environment) nil
	(buffers shader-environment) nil))

