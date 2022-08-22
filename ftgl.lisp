(in-package #:ftgl)

(cffi:define-foreign-library ftgl
  (:darwin "libftgl.dylib"))

(cffi:use-foreign-library ftgl)

(cffi:defcenum render-mode
  (:front 1)
  (:back 2)
  (:side 4)
  (:all #xffff))

(cffi:defcenum encoding
  (:none 0)
  (:unicode #x756e6963))

(cffi:defcfun (create-pixmap-font "ftglCreatePixmapFont") :pointer
  (font-name :string))

(cffi:defcfun (create-texture-font "ftglCreateTextureFont" ) :pointer
  (font-name :string))

(cffi:defcfun (create-extrude-font "ftglCreateExtrudeFont") :pointer
  (font-name :string))

(cffi:defcfun (set-font-face-size "ftglSetFontFaceSize") :void
  (font :pointer)
  (size :unsigned-int)
  (res :unsigned-int))

(cffi:defcfun (render-font "ftglRenderFont") :void
  (font :pointer)
  (string :string)
  (mode render-mode))

(cffi:defcfun (destroy-font "ftglDestroyFont") :void
  (font :pointer))



