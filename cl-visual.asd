(asdf:defsystem :cl-visual
  :serial t
  :depends-on (#:cl-nextstep
	       #:cl-glsl
	       #+sbcl :sb-concurrency
	       #-sbcl #:safe-queue)
  :components ((:file "package")
	       (:file "ftgl")
	       (:file "syphon")
	       (:file "gl-canvas")
	       (:file "renderer")
	       (:file "fps-info")
	       (:file "visual-canvas")
	       (:file "textures")
	       (:module "post-fx"
		:components ((:file "bloom")))))
