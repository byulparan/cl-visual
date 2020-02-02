(in-package :gfx)

;;; GL-Canvas
(defclass gl-canvas (gl-context)
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (camera :initarg :camera :reader camera)))

(defmethod init ((view gl-canvas)))
(defmethod draw ((view gl-canvas)))
(defmethod shutdown ((view gl-canvas)))

(export '(gl-canvas init draw shutdown)
	:gfx)
