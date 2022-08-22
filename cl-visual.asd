(asdf:defsystem :cl-visual
  :serial t
  :depends-on (:gfx-glsl
	       :cl-nextstep
	       :sc-internal
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
	       ;; (:file "cocoa-textures")
	       (:file "lib")
	       (:module "post-fx"
			:components ((:file "package")
				     (:file "bloom")))))
