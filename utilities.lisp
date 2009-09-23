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

(defun safe-shell-command (ignore-errors control-string &rest args)
  "Interpolate `args` into `control-string` as if by `format`, and
synchronously execute the result using a Bourne-compatible shell. Signal
an error on failure. Return a string consisting of the standard output (not the
standard error)"
  (let ((command (apply #'format `(nil ,control-string ,@args))))
    (multiple-value-bind (output error-output result)
        (trivial-shell:shell-command command)
      (if (and (not ignore-errors)
               (not (eql result 0)))
          (error "command '~a' returned error code ~a ~& ~a ~& ~a" command result output error-output))
      (values output result))))
