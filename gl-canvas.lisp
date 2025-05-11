(in-package :gfx)

;;; GL-Canvas
(defclass gl-canvas (shader-environment)
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (camera :initarg :camera :reader camera)
   (projection-matrix :accessor projection-matrix)
   (view-matrix :accessor view-matrix)))

(defmethod init ((view gl-canvas)))
(defmethod draw ((view gl-canvas)))
(defmethod reshape ((view gl-canvas)))
(defmethod release ((view gl-canvas)))

(defmethod release :after ((view gl-canvas))
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
