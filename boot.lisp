
;; load a known good version of asdf
(load (merge-pathnames (make-pathname :directory '(:relative ".." "asdf") :name "asdf")
                         *load-truename*))

;; clear output translations in case repo-install was already loaded in the world under a different username
;; (the output translations differ by user)
(asdf:clear-output-translations)

;; windows doen't support symbolic links, so we don't use them in repo-install
;; Set up the central registry temporarily so we can bootstrap repo-install
(let ((asdf:*central-registry*
       (append asdf:*central-registry*
		(mapcar #'(lambda (subdir)
			    (make-pathname :directory (append (butlast (pathname-directory *load-truename*))
							      (list subdir))
					   :device (pathname-device *load-truename*)))
			'("repo-install" "cl-fad" "trivial-shell" "cl-ppcre")))))
  (asdf:operate 'asdf:load-op :repo-install))

(load (merge-pathnames (make-pathname :name "most-recent-manifest" :type "lisp") *load-truename*))

(in-package :ri)

;; tell asdf how to load repo-install packages
(defun asd-file-for-system (system)
  (cond ((stringp system)
         (setq system (intern (string-upcase system) :keyword))))
  (let ((repo (find-repo system)))
    (if (null repo)
        (return-from asd-file-for-system nil))
    (cond ((not (asd-file repo :package-name system))
           (format t "Downloading ~a...~%" system)
           (update-repo repo)))
    (asd-file repo :package-name system)))

(pushnew 'asd-file-for-system asdf:*system-definition-search-functions*)
