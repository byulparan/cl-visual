(in-package :gfx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; draw-fbo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *fbo-stream* (gfx:make-gpu-stream '((pos :vec3) (coord :vec2))
					  (list -1.0 -1.0 0.0 0.0 0.0
						1.0 -1.0 0.0 1.0 0.0
						-1.0 1.0  0.0 0.0 1.0
						-1.0 1.0  0.0 0.0 1.0
						1.0 -1.0 0.0 1.0 0.0
						1.0 1.0 0.0 1.0 1.0)))

(gfx:defpipeline draw-fbo ((ichannel0 :sampler-2d-rect))
  (:vertex (:in ((pos :vec3) (coord :vec2)))
	   (setf v-coord coord)
	   (v! pos 1.0))
  (:fragment (:in ((v-coord :vec2)))
	     (texture ichannel0 (* v-coord (texture-size ichannel0)))))


;;; GL-Canvas
(defclass gl-canvas (shader-environment)
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (camera :initarg :camera :reader camera)
   (projection-matrix :accessor projection-matrix)
   (view-matrix :accessor view-matrix)
   (fbo :initarg :fbo :initform nil :accessor fbo)))

(defmethod init ((view gl-canvas)))
(defmethod draw ((view gl-canvas)))
(defmethod reshape ((view gl-canvas)))
(defmethod release ((view gl-canvas)))

(defmethod release :after ((view gl-canvas))
  (when (fbo view)
    (gfx:release-fbo (fbo view)))
  (gfx:release-environment view))



;; BitmapContext
(defclass bitmap-context ()
  ((context :initarg :context :accessor context)))


(defmethod init ((view bitmap-context)))
(defmethod draw ((view bitmap-context)))
(defmethod reshape ((view bitmap-context)))
(defmethod release ((view bitmap-context)))

(defmethod width ((view bitmap-context))
  (cg:context-width (context view)))

(defmethod height ((view bitmap-context))
  (cg:context-height (context view)))



(export '(gl-canvas bitmap-context context init draw reshape release projection-matrix view-matrix)
	:gfx)
