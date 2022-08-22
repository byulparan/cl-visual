(in-package :gfx)

(defstruct (shader-object
	    (:constructor %make-shader-object (programs shaders)))
  shaders)

(defun verify-file (shader-object)
  (let* ((shaders (shader-object-shaders shader-object)))
    (loop for sh in shaders
	  when sh
	    do (when (> (file-write-date (car sh)) (cdr sh))
		 (return-from verify-file t)))))

(defun make-shader-object (&key vertex geometry fragment)
  (assert (and vertex (setf vertex (probe-file vertex))))
  (when geometry
    (assert (setf geometry (probe-file geometry))))
  (when fragment
    (assert (setf fragment (probe-file fragment))))
  (%make-shader-object nil (list (cons vertex 0)
				 (if geometry (cons geometry 0))
				 (if fragment (cons fragment 0)))))


(defun update-shader-object (environment shader-object)
  (let* ((so shader-object)
	 (src (getf (shaders environment) shader-object)))
    (when (or (not src)
	      (verify-file so))
      (when src
	(let* ((program (second src))
	       (shaders (third src)))
	  (dolist (sh shaders)
	    (gl:detach-shader program sh)
	    (gl:delete-shader sh))
	  (gl:delete-program program)
	  (alexandria:remove-from-plistf (shaders environment) shader-object)))
      (let* ((success t)
	     (program (gl:create-program))
	     (shaders nil))
	(handler-case
	    (loop for sh in (shader-object-shaders shader-object)
		  for type in '(:vertex-shader :geometry-shader :fragment-shader)
		  when sh
		    collect (let* ((shader (gl:create-shader type)))
			      (alexandria:appendf shaders (list shader))
			      (gl:shader-source shader (with-output-to-string (var)
							 (with-open-file (stream (car sh))
							   (loop for line = (read-line stream nil nil)
								 while line
								 do (format var "~a~%" line)))))
			      (gl:compile-shader shader)
			      (unless (gl:get-shader shader :compile-status)
				(error (gl:get-shader-info-log shader)))
			      (gl:attach-shader program shader)))
	  (error (c) (progn (break (format nil "~a" c))
			    (dolist (sh shaders)
			      (gl:delete-shader sh))
			    (gl:delete-program program)
			    (setf success nil))))
	(when success
	  (setf (getf (shaders environment) shader-object) (list -1 program shaders))
	  (setf (shader-object-shaders so) (loop for sh in (shader-object-shaders so)
						 collect (when sh (cons (car sh) (file-write-date (car sh))))))
	  (gl:link-program program))))))

(defmacro with-shader ((program (environment shader-object stream)) &body body)
  `(progn
     (update-shader-object ,environment ,shader-object)
     (update-gpu-stream ,environment ,stream)
     (let* ((,program (cadr (getf (shaders ,environment) ,shader-object))))
       (when ,program
	 (gl:use-program ,program)
	 (gl:bind-vertex-array (second (getf (buffers ,environment) ,stream)))
	 ,@body
	 (gl:bind-vertex-array 0)
	 (gl:use-program 0)))))

