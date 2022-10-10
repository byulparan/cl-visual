(asdf:defsystem :cl-visual
  :serial t
  :depends-on (#:cl-nextstep
	       #:cl-glu
	       #:cl-glut
	       #:alexandria
	       #:cl-ppcre
	       #:sb-cga
	       #:split-sequence
	       #:mathkit
	       #+sbcl :sb-concurrency
	       #-sbcl #:safe-queue)
  :components ((:file "package")
	       (:module "glsl"
		:components ((:file "type")
			     (:file "compiler")
			     (:file "variables")
			     (:file "functions")
			     (:file "macros")
			     (:file "shader")
			     (:file "gfx/interface")
			     (:file "gfx/shader-environment")
			     (:file "gfx/gfx")
			     (:file "gfx/shader-object")
			     (:file "gfx/fbo")
			     (:file "gfx/camera")
			     (:file "gfx/read-wavefront")
			     (:file "gfx/mesh")))
	       (:file "ftgl")
	       (:file "syphon")
	       (:file "gl-canvas")
	       (:file "renderer")
	       (:file "fps-info")
	       (:file "visual-canvas")
	       (:file "textures")
	       ;; (:file "cocoa-textures")
	       (:file "lib")
	       (:module "post-fx"
		:components ((:file "bloom")))))
