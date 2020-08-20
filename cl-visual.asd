(asdf:defsystem :cl-visual
  :serial t
  :depends-on (:gfx-glsl
	       :cl-nextstep
	       :ftgl
	       :cl-syphon
	       :sc-internal
	       #+sbcl :sb-concurrency
	       #-sbcl #:safe-queue)
  :components ((:file "package")
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
