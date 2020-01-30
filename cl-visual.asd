(asdf:defsystem :cl-visual
  :serial t
  :depends-on (:gfx-glsl
	       :cl-nextstep
	       :ftgl
	       :sc-user
	       #+sbcl #:sb-concurrency
	       #+ccl #:safe-queue
	       #:cl-glu
	       #:cl-glut)
  :components ((:file "package")
	       (:file "renderer")
	       ;; (:file "fps-info")
	       ;; (:file "canvas-cocoa")
	       ;; (:file "textures")
	       ;; (:file "cocoa-textures")
	       ;; (:file "lib")
	       ))
