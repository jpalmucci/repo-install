
(defclass test-results ()
    ((repo :initarg :repo)
     (loaded)
     (load-output)
     (nfailed :initform nil)
     (ntests :initform nil)
     (test-output :initform nil))
)

(defmethod test-package ((r base-repo))
  (let ((result (make-instance 'test-results :repo r)))
    (with-slots (loaded load-output nfailed ntests test-output) result
      (with-slots (tester name) r
	(let ((*standard-output* (make-string-output-stream)))
	  ;; first load it
	  (handler-case
	      (progn
		(install name)
		(setf loaded t)
		(setf load-output (get-output-stream-string *standard-output*)))
	    (error (e)
	      (setf loaded nil)
	      (setf load-output (get-output-stream-string *standard-output*))
	      (return-from test-package result))))
	;; then test it if it has a test case
	(cond ((not (null tester))
	       (multiple-value-bind (output tests failures)
		   (funcall tester)
		 (setf nfailed failures)
		 (setf ntests tests)
		 (setf test-output output))))
	result))))



(defun test-all-repos (&optional (only-installed t))
  "For all repositories, print changes that have been made to the working
directory, but have not yet been committed to the local repository."
  (cond ((not only-installed)
	 (loop for package in (all-packages)
	      do (intall (name package)))))
  (loop for package in (all-packages)
     when (probe-file (working-dir package))
       collect (test-package package)))


(defun parse-rt-results (fn)
  (let ((*standard-output* (make-string-output-stream)))
    (ignore-errors
      (funcall fn))
    (multiple-value-bind (match submatch)
	(cl-ppcre:scan-to-strings (cl-ppcre:create-scanner "([0-9]+) out of ([0-9]+) total tests failed") (get-output-stream-string *standard-output*))
      (values (get-output-stream-string *standard-output*)
	      (parse-integer (aref submatch 0))
	      (parse-integer (aref submatch 1))))))

(defun parse-stefil-results (fn)
  (let ((*standard-output* (make-string-output-stream)))
    (ignore-errors
      (funcall fn))
    (multiple-value-bind (match submatch)
	(cl-ppcre:scan-to-strings (cl-ppcre:create-scanner
				   "([0-9]+) tests, [0-9]+ assertions, ([0-9]+) failures")
				  (get-output-stream-string *standard-output*))
      (values (get-output-stream-string *standard-output*)
	      (parse-integer (aref submatch 0))
	      (parse-integer (aref submatch 1))))))

(defun return-lift-results (r)
  (values ""
	  nil
	  (length (lift::failures r))))