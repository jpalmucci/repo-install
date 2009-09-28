
;; load the old version of asdf so we won't get an undefined function error
;; when sbcl tries to use ASDF::MODULE-PROVIDE-ASDF, which doesn't seem to be
;; in the newest release of asdf
#+sbcl
(require :asdf)

(load (merge-pathnames (make-pathname :directory '(:relative ".." "asdf") :name "asdf")
		       *load-truename*))

;; windows doen't support symbolic links, so we don't use them in repo-install
;; Set up the central registry temporarily so we can bootstrap repo-install
(let ((asdf:*central-registry* 
       (append asdf:*central-registry*
		(mapcar #'(lambda (subdir)
			    (make-pathname :directory (append (butlast (pathname-directory *load-truename*))
							      (list subdir))
					   :device (pathname-device *load-truename*)))
			'("repo-install" "cl-fad" "trivial-shell" "cl-ppcre")))))
  (asdf:load-system :repo-install))

(load (merge-pathnames (make-pathname :name "most-recent-manifest" :type "lisp") *load-truename*))

(in-package :ri) 

;; tell asdf how to load repo-install packages
(defun asd-file-for-system (system)
  (let ((repo (find-repo system)))
    (if (null repo)
        (return-from asd-file-for-system nil))
    (cond ((not (asd-file repo system))
           (format t "Downloading ~a...~%" system)
           (update-repo repo)))
    (asd-file repo system)))

(pushnew 'asd-file-for-system asdf:*system-definition-search-functions*)