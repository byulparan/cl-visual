(in-package :gfx)

;;; GL-Canvas
(defclass gl-canvas (shader-environment)
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (camera :initarg :camera :reader camera)
   (projection-matrix :accessor projection-matrix)
   (modelview-matrix :accessor modelview-matrix)))

(defmethod init ((view gl-canvas)))
(defmethod draw ((view gl-canvas)))
(defmethod release ((view gl-canvas)))

(export '(gl-canvas init draw release projection-matrix modelview-matrix)
	:gfx)
