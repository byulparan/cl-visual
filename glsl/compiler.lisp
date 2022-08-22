(in-package #:glsl)

(defvar *glsl-version* 330)

(defvar *stream*)
(defvar *context*)

(defvar *return-value*)
(defvar *global-functions*)

(defvar *structures*)

(defvar *variable-count*)
(defvar *variable-table* (make-hash-table))
(defvar *function-table* (make-hash-table))
(defvar *macro-table* (make-hash-table))


(defun symbol-gen (name)
  (format nil "~a_~a" (regex-replace-all "-" (string-downcase name) "_") (incf *variable-count*)))

(defun compile-form (form)
  (cond ((typep form 'code-object) form)
	((integerp form) (make-code-object :int form))
	((or (typep form 'double-float) (typep form 'long-float)) (make-code-object :float (format nil "~f" form)))
	((floatp form) (make-code-object :float form))
	((keywordp form) (error "not support keyword this form: ~a" form))
	((symbolp form) (case form
			  (true (make-code-object :bool "true"))
			  (false (make-code-object :bool "false"))
			  (t (let ((code-obj (gethash form *variable-table*)))
			       (unless code-obj (error "not found symbol \"~a\" in ~a function" form *context*))
			       (when (and (boundp '*used-global*) (use-global-p code-obj))
				 (pushnew form *used-global*))
			       code-obj))))
	((find (car form) '(let let*)) (%let (second form) (cddr form)))
	((eql (car form) 'defstruct) (push (%make-struct (second form) (cddr form)) *structures*))
	((eql (car form) 'progn) (%progn (cdr form)))
	((eql (car form) 'values) (mapcar #'compile-form (cdr form))) 
	((eql (car form) 'cond!) (%cond (cdr form)))
	((eql (car form) 'or) (%or (cdr form)))
	((eql (car form) 'and) (%and (cdr form)))
	((eql (car form) 'labels) (progn (push (%labels (second form))  *global-functions*)
					 (compile-form `,(cons 'progn (cddr form)))))
	((eql (car form) 'setq) (%setq (second form) (third form)))
	((eql (car form) 'return) (%return (second form)))
	((eql (car form) 'for) (%for (second form) (third form) (fourth form) (cddddr form)))
	((listp form) (let* ((name (car form)))
			(if-let ((macro-fun (gethash name *macro-table*)))
			  (apply macro-fun (cdr form))
			  (if-let ((fun (or (gethash (make-keyword name) *function-table*)
					    (gethash name *function-table*))))
			    (let ((obj (apply fun (mapcar #'compile-form (cdr form)))))
			      (when (and (boundp '*used-global*) (use-global-p obj))
				(pushnew name *used-global*))
			      obj)
			    (error "undefined function ~a" name)))))
	(t (error "what is it? it's not support cl-glsl: ~a" form))))

(defun %progn (forms)
  (when forms
    (dolist (form (butlast forms))
      (let ((obj (compile-form form)))
	(when-let ((line (code-line obj)))
	  (format *stream* "~a;~%" line))))
    (compile-form (last-elt forms))))

(defun function-args-parse (args)
  (labels ((args-parse (args)
	       (destructuring-bind (name type &optional (scope :in))
		   args
		 (assert (find scope (list :in :out :inout)))
		 (let ((newobj (make-code-object type (symbol-gen name) :write-p t)))
		   (setf (gethash name *variable-table*) newobj)
		   (format nil "~{~a~^ ~}" (list (string-downcase scope)
						 (code-type-name newobj)
						 (code-line newobj)))))))
    (format nil "(~{~a~^, ~})" (mapcar #'args-parse args))))

(defun packlized-name (name)
  (let* ((pkg (symbol-package name)))
    (string-downcase
     (regex-replace-all
      "\\."
      (regex-replace-all "-"
			 (format nil "~a_~a" (package-name pkg) name)
			 "_")
      "_"))))

(defun make-global-function (name args body)
  (assert (not (gethash (make-keyword name) *function-table*)) nil "~s is glsl core function" name)
  (let* ((*context* name)
	 (*variable-table* (copy-hash-table *variable-table*))
	 (*variable-count* 11)
	 (fmt-args (function-args-parse args))
	 (function-name (packlized-name name))
	 (body-form nil)
	 (body-return nil))
    (let* ((*return-value* nil))
      (setf body-form
	(with-output-to-string (*stream*)
	  (setf body-return (eval `(compile-form ',(cons 'progn body))))
	  (if (member body-return *return-value*)  ; 만약 body-return 이 return 함수라면..
	      (format *stream* "~a;~%" (code-line body-return))
	    (format *stream* "~@[return ~a;~%~]" (code-line body-return)))))
      (dolist (r *return-value*)
	(assert (eql (code-type body-return) (code-type r)))))
    (setf (gethash name *function-table*)
      (let* ((rename-args (mapcar (lambda (arg) (intern (format nil "~a_D" (first arg)))) args)))
	(eval
	 `(lambda ,rename-args
	    (assert (equal ',(mapcar #'second args) (mapcar #'code-type ,(cons 'list rename-args))))
	    ,@(loop for name in rename-args
		    for scope in (mapcar (lambda (arg) (third arg)) args)
		    collect  (when (and scope (not (eql scope :in)))
			       `(assert (write-p ,name))))
	    (make-code-object (code-type ,body-return)
			      (format nil "~a(~{~a~^,~})" ,function-name
				      (mapcar #'code-line ,(cons 'list rename-args)))
			      :use-global-p t)))))
    (let* ((return-type (code-type-name body-return)))
      (when (boundp '*return-value*)
	(setf *return-value* (make-keyword (string-upcase return-type))))
      (format *stream* "~a ~a ~a {~%~{~2t~a~%~}}"
	      return-type
	      function-name
	      fmt-args
	      (butlast (split-sequence #\Newline body-form))))))

(defun make-global-variable (name value)
  (let* ((*context* name)
	 (obj (compile-form value)))
    (when (eql :void (code-type obj))
      (error "\"~s\" is not expression(maybe if,for) or void return form!" obj))
    (let* ((code-name (packlized-name name))
	   (object (if (typep obj 'code-object-array) (make-instance 'code-object-array
							:base-type (base-type obj)
							:code-type (code-type obj)
							:code-line code-name
							:use-global-p t
							:write-p t
							:size (size obj))
		     (make-code-object (code-type obj) code-name :write-p t
								 :use-global-p t))))
      (setf (gethash name *variable-table*) object)
      (when (boundp '*return-value*)
	(setf *return-value* (code-type object)))
      (format *stream* "~a ~a~@[ = ~a~];"
	      (code-type-name obj)
	      (if (typep obj 'code-object-array) (format nil "~a[~a]" code-name (size obj)) code-name)
	      (code-line obj)))))

(defun %labels (functions)
  (with-output-to-string (*stream*)
    (format *stream* "~%")
    (dolist (func functions)
      (if (> (length func) 2) (destructuring-bind (name arg &rest body)
				  func
				(when (gethash name *macro-table*)
				  (error "\"~a\" is pre-defined via v-defmacro.You can't define function use this name." name))
				(make-global-function name arg body))
	  (destructuring-bind (name value)
	      func
	    (make-global-variable name value)))
      (unless (boundp '*return-value*)
	(format *stream* "~%~%")))))


(defun %let (binding body)
  (assert body nil "\"LET\" form should be has body.")
  (let* ((*variable-table* (copy-hash-table *variable-table*)))
    (dolist (b binding)
      (destructuring-bind (name form)
	  b
	(let* ((var-name (symbol-gen name))
	       (obj (compile-form form)))
	  (when (or (not (expression-p obj)) (eql :void (code-type obj)))
	    (error "\"~s\" is not expression(maybe if,for) or void return form!" form))
	  (setf (gethash name *variable-table*) (if (typep obj 'code-object-array)
					       (make-instance 'code-object-array
						 :base-type (base-type obj)
						 :code-type (code-type obj)
						 :code-line var-name
						 :use-global-p nil
						 :write-p t
						 :size (size obj))
					     (make-code-object (code-type obj) var-name :write-p t)))
	  (format *stream* "~&~a ~a~@[ = ~a~];~%"
		  (code-type-name obj)
		  (if (typep obj 'code-object-array) (format nil "~a[~a]" var-name (size obj)) var-name)
		  (code-line obj)))))
    (when body
      (dolist (b (butlast body))
	(let* ((obj (compile-form b)))
	  (when-let ((line (code-line obj)))
	    (format *stream* "~a;~%" line))))
      (let ((return-obj (compile-form (last-elt body))))
	return-obj))))

(defun %setq (name form)
  (let ((var (compile-form name))
	(code (compile-form form)))
    (assert (expression-p code) nil "\"~a\" is not expression." form)
    (assert (equal (code-type var) (code-type code)) nil "~a is difference ~a." (code-type var) (code-type code))
    (let ((array-p (mapcar #'(lambda (obj) (typep obj 'code-object-array)) (list var code))))
      (assert (apply #'eql array-p) nil
	      "non array mismatch \"~a, ~a\"" (code-line var) (code-line code))
      (when (car array-p)
	(assert (= (size var) (size code)) nil "array size mismatch with \"~a, ~a\"" (code-line var) (code-line code)))) 
    (unless (write-p var) (error "\"~a\" is not writable." name))
    (make-code-object (code-type var) (format nil "~&(~a = ~a)" (code-line var) (code-line code)))))

(defun %cond (clause)
  (labels ((parsing-form (key test-form form)
	     (let* ((output-code nil))
	       (when test-form
		 (setf test-form (compile-form test-form))
		 (assert (eql (code-type test-form) :bool)))
	       (let* ((parse-code-line
			(split-sequence #\Newline
					(with-output-to-string (*stream*)
					  (setf output-code (compile-form form))))))
		 (when (string= (last-elt parse-code-line) "")
		   (setf parse-code-line (butlast parse-code-line)))
		 (format *stream* "~a ~@[~a ~]{~%~{~2t~a~%~}~@[~2t~a;~%~]}~%"
			 key
			 (when test-form
			   (let* ((line (code-line test-form)))
			     (if (char= #\( (elt line 0)) line
			       (format nil "(~a)" line))))
			 parse-code-line
			 (when output-code (code-line output-code)))))))
    (let ((c (car clause)))
      (parsing-form "if" (first c) `(progn ,@(cdr c))))
    (loop for c in (butlast (cdr clause))
	  do (parsing-form "else if" (first c) `(progn ,@(cdr c))))
    (let* ((c (car (last (cdr clause))))
	   (else-p (eq t (car c))))
      (when c
	(parsing-form (if else-p "else" "else if") (if else-p nil (first c)) `(progn ,@(cdr c))))))
  (make-code-object :void nil))

(defun %or (form)
  (let* ((codes (mapcar #'compile-form form)))
    (assert (every (lambda (a) (eql a :bool)) (mapcar #'code-type codes)))
    (make-code-object
     :bool
     (format nil "(~{~a~^ ~^||~^ ~})" (mapcar #'code-line codes)))))

(defun %and (form)
  (let* ((codes (mapcar #'compile-form form)))
    (assert (every (lambda (a) (eql a :bool)) (mapcar #'code-type codes)))
    (make-code-object
     :bool
     (format nil "(~{~a~^ ~^&&~^ ~})" (mapcar #'code-line codes)))))

(defun %for (binding until continue body)
  (let* ((*variable-table* (copy-hash-table *variable-table*)))
    (destructuring-bind (name form)
	binding
      (let ((form (compile-form form)))
	(setf (gethash name *variable-table*) (make-code-object (code-type form) (symbol-gen name)
							     :write-p t))
	(let* ((until (compile-form until))
	       (continue (when continue (compile-form continue))))
	  (setf body (loop for b in body
			   for obj = nil
			   append (let ((parse-line
					  (with-output-to-string (*stream*)
					    (let ((b (compile-form b)))
					      (when-let ((line (code-line b)))
						(setf obj (format nil "~a;~%" line)))))))
				    (list parse-line obj)))
		body (remove
		      ""
		      (split-sequence
		       #\Newline
		       (apply #'concatenate 'string body))
		 :test #'string=))
	  (format *stream* "~&for(~a ~a = ~a; ~a; ~@[~a~]) {~%~{~2t~a~%~}}~%"
		  (code-type-name form)
		  (code-line (compile-form name))
		  (code-line form)
		  (code-line until)
		  (when continue (code-line continue))
		  body)))))
  (make-code-object :void nil :expression-p nil))

(defun %return (form)
  (let* ((form (when form (compile-form form))))
    (let ((obj (make-code-object (if form (code-type form) :void) 
				 (format nil "return~@[ ~a~]" (if form (code-line form)))
				 :expression-p nil)))
      (pushnew obj *return-value*)
      obj)))



(defun struct-form (name form)
  (with-output-to-string (stream)
    (let* ((type-name (packlized-name name)))
      (format stream "~%struct ~a {~%" (ppcre:regex-replace-all "-" type-name "_")))
    (loop for (name type size) in form
	  do (format stream "~2t~a ~a~@[[~a]~];~%"
		     (process-type-name type)
		     (ppcre:regex-replace-all "-" (string-downcase name) "_")
		     size))
    (format stream "};~%")))

(defun %make-struct (name form)
  (setf form (mapcar #'(lambda (f) (list (first f) (eval (second f)) (third f))) form))
  (let* ((glsl-name name)
	 (args-name (mapcar #'first form))
	 (args-type (mapcar #'(lambda (arg) (if (third arg) (list :array (second arg) (third arg))
					      (second arg))) form))
	 (args-size (mapcar #'(lambda (arg) (third arg)) form)))
    (pushnew glsl-name *all-glsl-type*)
    (loop for slot-name in args-name
	  for slot-type in args-type
	  for slot-size in args-size
    	  do (let* ((regist-name (intern (string-upcase (format nil "~a-~a" name slot-name)) (symbol-package name)))
		    (slot-name (ppcre:regex-replace-all "-" (string-downcase slot-name)  "_"))
		    (slot-type slot-type)
		    (slot-size slot-size)
		    (base-type (if (consp slot-type) (second slot-type) slot-type)))
	       (when (not (keywordp base-type)) (pushnew base-type *used-global*))
    	       (setf (gethash regist-name *function-table*)
    		 (lambda (object)
    		   (assert (eql (code-type object) glsl-name) nil "wrong slot accessor ~a::~a for struct ~a::~a"
			   (package-name (symbol-package regist-name)) regist-name
			   (package-name (symbol-package (code-type object))) (code-type object))
		   (if slot-size
		       (make-instance 'code-object-array
			   :base-type base-type
			   :code-type slot-type
			   :code-line (format nil "~a.~a" (code-line object) slot-name)
			   :write-p t
			   :size slot-size)
		     (make-code-object base-type
				       (format nil "~a.~a" (code-line object) slot-name)
				       :read-p t :write-p t)))))))
  (struct-form name form))
