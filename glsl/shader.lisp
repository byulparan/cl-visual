(in-package :glsl)

(defstruct shader
  version type io-spec uniforms source function)

(defmethod process-input-vertex ((version (eql 120)) variable-obj count)
  (declare (ignore count))
  (format *stream* "attribute ~a ~a;~%"
	  (code-type-name variable-obj) (code-line variable-obj)))

(defmethod process-input-vertex ((version (eql 330)) variable-obj count)
  (format *stream* "layout(location=~d) in ~a ~a;~%"
	  count (code-type-name variable-obj) (code-line variable-obj)))

(defun process-input-geometry (name var-name type)
  (format *stream* "in ~a ~a[];~%" (process-type-name type) var-name)
  (setf (gethash name *variable-table*) 
    (make-instance 'code-object-array
      :size 100000 ;; dummy 
      :code-type (list :array type 100000)
      :base-type type
      :code-line var-name
      :read-p t
      :write-p nil)))

(defun process-spec-geometry (shader)
  (let* ((layout (getf (shader-io-spec shader) :layout)))
    (assert layout nil "geometry shader should have layout")
    (destructuring-bind (input-type output-type max-vertices)
	layout
      (format *stream* "layout (~a) in;~%" (ecase input-type
					     (:points "points")
					     (:lines "lines")
					     (:lines-adjacency "lines_adjacency")
					     (:triangles "triangles")
					     (:triangles-adjacency "triangles_adjacency")))
      (format *stream* "layout (~a, max_vertices=~a) out;~%" (ecase output-type
							       (:points "points")
							       (:line-strip "line_strip")
							       (:triangle-strip "triangle_strip"))
	      (floor max-vertices)))))

(defun process-input (shader)
  (let* ((location-count 0)
	 (version (shader-version shader)))
    (loop for input in (getf (shader-io-spec shader) :in)
	  do (destructuring-bind (name type)
		 input
	       (let ((var-name (regex-replace-all "-" (string-downcase name) "_")))
		 (when (gethash name *variable-table*)
		   (error "already has name input var \"~a\"" name))
		 (case (shader-type shader)
		   (:vertex (let ((newobj (setf (gethash name *variable-table*) (make-code-object type var-name))))
			      (progn (process-input-vertex version newobj location-count)
				     (incf location-count))))
		   (:geometry (process-input-geometry name var-name type))
		   (:fragment (let ((newobj (setf (gethash name *variable-table*) (make-code-object type var-name)))
				    (fmt (ecase version
					   (120 "varying ~a ~a;~%")
					   (330 "in ~a ~a;~%"))))
				(format *stream* fmt
					(code-type-name newobj)
					(code-line newobj))))))))))

(defun process-output (shader)
  (let* ((location-count 0)
	 (version (shader-version shader)))
    (when (eql (shader-type shader) :fragment)
      (setf (getf (shader-io-spec shader) :out)
	(list (list (intern (string-upcase (format nil "OUTPUT_~a_COLOR" (gensym)))) :vec4))))
    (loop for output in (getf (shader-io-spec shader) :out)
	  collect (destructuring-bind (name type)
		      output
		    (let ((var-name (regex-replace-all "-" (string-downcase name) "_")))
		      (let ((out (make-code-object type var-name :write-p t)))
			(setf (gethash name *variable-table*) out)
			(ecase (shader-type shader)
			  (:vertex (let* ((fmt (ecase version
						 (120 "varying ~a ~a;~%")
						 (330 "out ~a ~a;~%"))))
				     (format *stream* fmt
					     (code-type-name out) (code-line out))))
			  (:geometry
			   (format *stream* "out ~a ~a;~%" (code-type-name out) (code-line out)))
			  (:fragment (when (= version 330)
				       (format *stream* "layout(location = ~d) out ~a ~a;~%"
					       location-count
					       (code-type-name out) (code-line out))
				       (incf location-count))))
			out))))))

(defun process-uniform (shader)
  (loop for uniform in (shader-uniforms shader)
	do (destructuring-bind (name type)
	       uniform
	     (when (gethash name *variable-table*)
	       (error "already has name uniform var \"~a\"" name))
	     (let* ((var-name (regex-replace-all "-" (string-downcase name) "_"))
		    (newobj (make-code-object type var-name)))
	       (setf (gethash name *variable-table*) newobj)
	       (format *stream* "uniform ~a ~a;~%" (code-type-name newobj) (code-line newobj))))))

(defun process-shader (shader)
  (let* ((*glsl-version* (shader-version shader))
	 (*variable-count* 0)
	 (*structures* nil)
	 (*global-functions* nil)
	 (shader-type (shader-type shader))
	 (*all-glsl-type* (copy-list *all-glsl-type*))
	 (*function-table* (copy-hash-table *function-table*))
	 (*variable-table* (copy-hash-table *variable-table*))
	 (*context* shader-type)
	 (outputs nil)
	 (result-code nil))
    (with-output-to-string (*stream*)
      (format *stream* "~%")
      (format *stream* "#version ~d~%~%" (shader-version shader))
      (when (eql shader-type :geometry)
	(process-spec-geometry shader))
      (process-input shader)
      (setf outputs (process-output shader))
      (process-uniform shader)
      (let* ((main-function (with-output-to-string (*stream*)
			      (setf result-code (compile-form (shader-source shader)))
			      (if (eql shader-type :geometry) (format *stream* "~a;~%" (code-line result-code))
				(progn
				  (assert (eql :vec4 (code-type result-code)))
				  (format *stream* "~a = ~a;~%" (ecase shader-type
								  (:vertex "gl_Position")
								  (:fragment (ecase (shader-version shader)
									       (120 "gl_FragColor")
									       (330 (with-slots (code-line) (car outputs)
										      code-line)))))
					  (code-line result-code)))))))
	(when (shader-function shader)
	  (funcall (shader-function shader) *stream*))
	(format *stream* "~%")
	(format *stream* "~@[~%~{~a~}~%~]~@[~{~a~}~]void main () {~%~{~2t~a~%~}}"
		(nreverse *structures*)
		(nreverse *global-functions*)
		(butlast (split-sequence #\Newline main-function)))))))



(defmacro defpipeline (name args &body body)
  (when (atom name) (setf name (list name :version *glsl-version*)))
  (let* ((version (third name))
	 (varyings (second (second body)))
	 (uniforms args))
    (assert (every (lambda (body) (= 3 (length body))) body) nil "each stages sholud be have 3 element(type, io-spec, body)")
    `(mapcar #'process-shader
	     (mapcar #'(lambda (form) (make-shader :version ,version
						   :type (first form)
						   :io-spec (second form)
						   :uniforms ',uniforms
						   :source (third form)))
		     ',body))))
