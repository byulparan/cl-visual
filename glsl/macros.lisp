(in-package :glsl)

(defmacro destructuring-defun (args &body body)
  (let* ((pos (position-if (lambda (a) (find a '(&optional &key &rest &body))) args)))
    (if (not pos) (setf pos 0))
    `(lambda ,(nconc (mapcar #'ensure-car (subseq args 0 pos)) (subseq args pos))
       ,(multiple-value-bind (form count)
	    (loop for arg in (reverse (subseq args 0 pos))
		  with result = body
		  with count = 0
		  if (listp arg)
		    do (let ((dest-form `(destructuring-bind ,arg
					     ,(ensure-car arg))))
			 (setf result (nconc dest-form (if (zerop count) result (list result))))
			 (incf count))
		  finally (return (values result count)))
	  (if (zerop count) `(progn ,@form) form)))))

(defmacro v-defmacro (name args &body body)
  (when (gethash (symbol-name name) *function-table*)
    (error "\"~a\" is glsl built-in function. You can't define macro use this name." name))
  `(let* ((fun 
	    (destructuring-defun ,(nsubst '&rest '&body args)
	      (compile-form ,@body))))
     (setf (gethash ',name *macro-table*) fun)))


(v-defmacro get (object accessor &optional return-type)
  (let* ((obj (glsl::compile-form object)))
    (warn "get function depredcated. use s~ instead.")
    (cond ((symbolp accessor) (let* ((idx (string-downcase accessor)))
				(make-code-object (ecase (length idx)
						    (1 :float)
						    (2 :vec2)
						    (3 :vec3)
						    (4 :vec4))
						  (format nil "~a.~a" (code-line obj) idx) :write-p (write-p obj))))
	  ((and return-type (stringp accessor)) (make-code-object return-type (format nil "~a.~a" (code-line obj) accessor) :write-p t))
	  (t (error "can't swizzle used get")))))


(v-defmacro s~ (object accessor &optional return-type)
  (let* ((obj (glsl::compile-form object)))
    (cond ((symbolp accessor) (let* ((idx (string-downcase accessor)))
				(make-code-object (ecase (length idx)
						    (1 :float)
						    (2 :vec2)
						    (3 :vec3)
						    (4 :vec4))
						  (format nil "~a.~a" (code-line obj) idx) :write-p (write-p obj))))
	  ((and return-type (stringp accessor)) (make-code-object return-type (format nil "~a.~a" (code-line obj) accessor) :write-p t))
	  (t (error "can't swizzle used get")))))




(v-defmacro lisp (form)
  (eval form))

(v-defmacro setf (&body body)
  `(progn
    ,@(loop for (var form) on body by #'cddr
	    collect `(setq ,var ,form))))

(v-defmacro loop (&body body)
  (let* ((body (destructuring-bind (for var _from from cond end _do &rest body)
		   body
		 for _from _do
		 `(for (,var ,from) (,(if (eql (make-keyword cond) :below) '< '<=) ,var ,end)
		       (setf ,var (+ ,var 1))
		       ,@body))))
    (assert body)
    body))

(v-defmacro dotimes ((var n) &body body)
  `(loop for ,var from ,(ecase (code-type (compile-form n))
			  (:int 0)
			  (:float 0.0))
	   below ,n
	 do ,@body))

(v-defmacro incf (var &optional (i 1))
  `(setf ,var (+ ,var ,i)))

(v-defmacro decf (var &optional (i 1))
  `(setf ,var (- ,var ,i)))

(v-defmacro multf (var i)
  `(setf ,var (* ,var ,i)))


(v-defmacro if! (test-form true-form &optional false-form)
  `(cond! (,test-form ,true-form)
	 ,(when false-form `(t ,false-form))))

(v-defmacro if (condition true &optional false)
  (let* ((obj (let* ((*stream* nil)) (compile-form true)))
	 (type (code-type obj))
	 (result (gensym)))
    (if (or (not (expression-p obj)) (eql type :void)) `(if! ,condition ,true ,false)
	`(let* ((,result (initialize ,type)))
	   (if! ,condition (setf ,result ,true)
		,(when false
		   `(setf ,result ,false)))
	   ,result))))

(v-defmacro cond (&body claus)
  (let* ((obj (let* ((*stream* nil)) (compile-form (cons 'progn (cdar claus)))))
	 (type (code-type obj))
	 (result (gensym)))
    (if (or (not (expression-p obj)) (eql type :void)) `(cond! ,@claus)
      `(let* ((,result (initialize ,type)))
	 (cond! ,@(mapcar (lambda (form) `(,(car form) (setf ,result (progn ,@(cdr form))))) claus))
	 ,result))))

(v-defmacro when (test-form &body body)
  `(if ,test-form (progn ,@body)))

(v-defmacro make-array (base-type size)
  (progn
    (assert (integerp size) nil "array allocation size should be int not ~a" size)
    (setf base-type (eval base-type))
    (make-instance 'code-object-array
      :size size
      :code-type (list :array base-type size)
      :base-type base-type
      :code-line nil
      :read-p nil
      :write-p nil)))

;;; type initializer
(v-defmacro initialize (base-type)
  (make-instance 'code-object
		  :code-type (eval base-type)
		  :code-line nil))

