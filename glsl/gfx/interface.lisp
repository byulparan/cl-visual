(in-package :gfx)

;;; generic fucntion
(defgeneric width (context))
(defgeneric height (context))


;;; GPU stream
(defstruct %gpu-stream
  types
  names
  core-profile
  array
  index-array
  length
  update-time
  info)


(defmethod print-object ((entry %gpu-stream) stream)
  (format stream "#S(%GPU-STREAM :TYPE ~a :CORE-PROFILE ~a :LENGTH ~d)"
	  (%gpu-stream-types entry)
	  (%gpu-stream-core-profile entry)
	  (%gpu-stream-length entry)))


;;; pipeline
(defstruct %pipeline
  version
  name
  shader-src
  vertex-src
  geometry-src
  fragment-src
  uniforms
  (used-funcs nil)
  update-time)

(defmethod print-object ((entry %pipeline) stream)
  (format stream "#S(%PIPELINE :NAME ~a)"  (%pipeline-name entry)))
