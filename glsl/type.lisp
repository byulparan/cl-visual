(in-package :glsl)


(defvar *used-global*)

(defvar *default-glsl-type*
  (list :void :bool
	:float :int
	:mat2 :mat3 :mat4 :vec2 :vec3 :vec4 :ivec2 :ivec3 :ivec4
	:bvec2 :bvec3 :bvec4
	:sampler-2d :sampler-2d-rect :sampler-cube))

(defvar *all-glsl-type* (copy-list *default-glsl-type*))

(defvar *binding-form* nil)

(defclass code-object ()
  ((code-line :initarg :code-line :reader code-line)
   (code-type :initarg :code-type :initform (error "no allocation this type") :reader code-type)
   (readtable-p :initarg :read-p :reader read-p)
   (writable-p :initarg :write-p :reader write-p)
   (use-global-p :initarg :use-global-p :initform nil :reader use-global-p)
   (expression-p :initarg :expression-p :initform t :reader expression-p)
   (meta-info :initarg :meta-info :initform nil :reader meta-info)))

(defmethod print-object ((c code-object) stream)
  (format stream "#<CODE-OBJECT ~s~@[ ~s~]>" (code-type c) (code-line c)))

(defclass code-object-array (code-object)
  ((base-type :initarg :base-type :reader base-type)
   (size :initarg :size :reader size)))

(defmethod print-object ((c code-object-array) stream)
  (format stream "#<CODE-OBJECT-ARRAY ~s[~a]>" (code-type c) (size c)))

(defmethod initialize-instance :after ((self code-object-array) &key)
  (unless (member (base-type self) *default-glsl-type*)
    (when (boundp '*used-global*)
      (pushnew (base-type self) *used-global*))))

(defmethod code-len ((code-object code-object))
  (ecase (code-type code-object)
    (:bool 1)
    (:int 1)
    (:float 1)
    ((:vec2 :ivec2 :bvec2) 2)
    ((:vec3 :ivec3 :bvec3) 3)
    ((:vec4 :ivec4 :bvec4) 4)
    (:mat2 4)
    (:mat3 9)
    (:mat4 16)))

(defmethod code-len ((code-object code-object-array))
  (size code-object))

(defun process-type-name (code-type)
  (assert (member code-type *all-glsl-type*) nil "can't found ~s type" code-type)
  (case code-type
    (:sampler-2d "sampler2D")
    (:sampler-2d-rect "sampler2DRect")
    (:sampler-cube "samplerCube")
    (t (let* ((type-name (if (keywordp code-type) (string-downcase code-type) ;; 코어 타입
			   ;; 구조체(유저 정의) 타입
			   (string-downcase (format nil "~a_~a"
						    (package-name (symbol-package code-type)) code-type)))))
	 (ppcre:regex-replace-all "-" type-name "_")))))

(defmethod code-type-name ((code-object code-object))
  (process-type-name (code-type code-object)))

(defmethod code-type-name ((code-object code-object-array))
  (process-type-name (base-type code-object)))

(defun vector-p (code-object)
  (member (code-type code-object) '(:vec2 :vec3 :vec4)))

(defun ivector-p (code-object)
  (member (code-type code-object) '(:ivec2 :ivec3 :ivec4)))

(defun bvector-p (code-object)
  (member (code-type code-object) '(:bvec2 :bvec3 :bvec4)))

(defun number-p (code-object)
  (member (code-type code-object) '(:int :float)))

(defun matrix-p (code-object)
  (member (code-type code-object) '(:mat2 :mat3 :mat4)))

(defun make-code-object (type code-line &key (read-p t) (write-p nil) (use-global-p nil)
					  (expression-p t))
  (unless (member type *default-glsl-type*)
    (if (boundp '*used-global*) (pushnew type *used-global*)
      (error "can't found ~s type" type)))
  (make-instance 'code-object
    :code-line code-line
    :code-type type
    :use-global-p use-global-p
    :read-p read-p
    :write-p write-p
    :expression-p expression-p))


