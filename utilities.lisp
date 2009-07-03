(in-package :ri)

(defun parent-directory (dirpath)
  (make-pathname :directory (let ((dir (pathname-directory dirpath)))
                              (cons (car dir) (butlast (cdr dir))))))

(defun dir-as-file (x)
  "trim the trailing /"
  (make-pathname :directory (butlast (pathname-directory x)) :name (car (last (pathname-directory x)))))

(defun makedirs (dir)
  (cond ((not (probe-file dir))
	 (if (null (pathname-name dir))
	     (setq dir (make-pathname
			:directory (butlast (pathname-directory dir))
			:name (car (last (pathname-directory dir))))))
	 (safe-shell-command nil "mkdir -p ~a" dir))))

(defun files-different (a b)
  (multiple-value-bind (result code)
      (safe-shell-command t "diff --brief ~a ~a" a b)
    (not (eql code 0))))

(defmacro delete-dir-on-error (dir &body body)
  `(handler-case
       (progn ,@body)
     (error (e)
       (cl-fad:delete-directory-and-files ,dir :if-does-not-exist :ignore)
       (error e))))
     

;; shamelessly copied from asdf
(defun safe-shell-command (ignore-errors control-string &rest args)
  "Interpolate `args` into `control-string` as if by `format`, and
synchronously execute the result using a Bourne-compatible shell. Signal
an error on failure. Return a string consisting of the standard output (not the
standard error)"
  (let ((result nil))
    (values 
     (with-output-to-string (output-stream)
       (let ((command (apply #'format nil control-string args)))
	 (setq result 
	       (progn
		 #+sbcl
		 (sb-ext:process-exit-code
		  (sb-ext:run-program
		   #+win32 "sh" #-win32 "/bin/sh"
		   (list  "-c" command)
		   #+win32 #+win32 :search t
		   :input nil :output output-stream))

		 #+(or cmu scl)
		 (ext:process-exit-code
		  (ext:run-program
		   "/bin/sh"
		   (list  "-c" command)
		   :input nil :output output-stream))

		 #+allegro
		 (excl.osi:with-command-io (command)
		   (:output (o)
			    (princ o output-stream)
			    (terpri output-stream))
		   (:error-output (e)
				  (princ e output-stream)
				  (terpri output-stream)))
		 #+ignore
		 ;; will this fail if command has embedded quotes - it seems to work
		 (multiple-value-bind (stdout stderr exit-code)
		     (excl.osi:command-output 
		      (format nil "~a -c \"~a\"" 
			      #+mswindows "sh" #-mswindows "/bin/sh" command)
		      :input nil :whole nil
		      #+mswindows :show-window #+mswindows :hide)
		   (format output-stream "~{~&; ~a~%~}~%" stderr)
		   (format output-stream "~{~&; ~a~%~}~%" stdout)
		   exit-code)

		 #+lispworks
		 (system:call-system-showing-output
		  command
		  :shell-type "/bin/sh"
		  :output-stream output-stream)

		 #+clisp	;XXX not exactly output-stream, I know
		 (ext:run-shell-command  command :output :terminal :wait t)

		 #+(or ccl openmcl)
		 (nth-value 1
			    (ccl:external-process-status
			     (ccl:run-program "/bin/sh" (list "-c" command)
					      :input nil :output output-stream
					      :wait t)))

		 #+ecl ;; courtesy of Juan Jose Garcia Ripoll
		 (si:system command)

		 #-(or openmcl clisp lispworks allegro scl cmu sbcl ecl)
		 (error "SAFE-SHELL-COMMAND not implemented for this Lisp")
		 ))
	 (if (and (not ignore-errors)
		  (not (eql result 0)))
	     (error "command '~a' returned error code ~a ~& ~a" command result (get-output-stream-string output-stream)))))
     result)))


