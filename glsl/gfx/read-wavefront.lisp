(in-package :gfx)

(defun parse-faces (line vb vtb vnb index-info)
  (flet ((rfs (s)
	   (if (zerop (length s)) nil
	     (1- (read-from-string s)))))
    (loop for l in (cdr line)
	  collect (let* ((seq (split-sequence:split-sequence #\/ l))
			 (indexed (mapcar #'rfs seq)))
		    (let* ((idx (gethash indexed (second index-info))))
		      (if idx (list nil idx)
			(destructuring-bind (v vt vn)
			    indexed
			  (incf (car index-info))
			  (setf (gethash indexed (second index-info)) (car index-info))
			  (list
			   (append (elt vb v) (if vt (elt vtb vt) nil) (elt vnb vn))
			   (car index-info)))))))))

(defun read-obj-file (objfile)
  (with-open-file (stream objfile)
    (let* ((vertex (make-array 0 :adjustable t :fill-pointer t))
	   (normal (make-array 0 :adjustable t :fill-pointer t))
	   (index-info (list -1 (make-hash-table :test #'equalp)))
	   (result-index nil)
	   (result-vector nil))
      (loop for line = (read-line stream nil nil)
	    while line
	    do (let ((seq (split-sequence:split-sequence #\space line)))
		 (when (find (car seq) '("v" "vn") :test #'string=)
		   (vector-push-extend
		    (mapcar #'read-from-string (cdr seq))
		    (if (string= (car seq) "v") vertex normal)))
		 (when (string= (car seq) "f")
		   (assert (= 3 (length (cdr seq))) nil "not triangulated")
		   (let* ((result (parse-faces seq vertex nil normal index-info)))
		     (setf result-vector (append result-vector (apply #'append (mapcar #'first result))))
		     (setf result-index (append result-index (mapcar #'second result)))))))
      (gfx:make-gpu-stream '((position :vec3) (normal :vec3)) result-vector
			   :index-data result-index
			   :core-profile t))))



