(in-package :glsl)

;;; vertex shader
(setf (gethash 'gl-vertex-id *variable-table*) (make-code-object :int "gl_VertexID"))
(setf (gethash 'gl-instance-id *variable-table*) (make-code-object :int "gl_InstanceID"))
(setf (gethash 'gl-point-size *variable-table*) (make-code-object :float "gl_PointSize" :write-p t))

;; geometry shader
(setf (gethash 'gl-position *variable-table*) (make-code-object :vec4 "gl_Position" :write-p t))


;;; fragment shader
(setf (gethash 'gl-frag-coord *variable-table*) (make-code-object :vec4 "gl_FragCoord"))
(setf (gethash 'gl-point-coord *variable-table*) (make-code-object :vec2 "gl_PointCoord"))
(setf (gethash 'gl-frag-depth *variable-table*) (make-code-object :float "gl_FragDepth" :write-p t))

;; gl_DepthRange is glsl built-in structure(gl_DepthRangeParameters). but I just dummy use :void
(setf (gethash 'gl-depth-range *variable-table*) (make-code-object :void "gl_DepthRange"))


