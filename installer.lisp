(in-package :ri)

(defun test-environment (repo-type)
  (ecase repo-type 
    (:git
	  (if (null *git-location*)
	      (error "git executable not found, required to get the package source")))
    (:darcs
     (if (null *darcs-location*)
	 (error "darcs executable not found, required to get the package source")))
    (:svn
     (if (null *svn-location*)
	 (error "svn executable not found, required to get the package source")))
    (:bzr
     (if (null *bzr-location*)
	 (error "bzr executable not found, required to get the package source")))))
	  

(defun install (package)
  (handler-case
      (asdf:operate 'asdf:load-op package)
    (asdf:missing-component (c)
      (let ((repo (find-repo (asdf::missing-requires c))))
	(cond ((null repo)
	       (error "Don't know how to get the ~a package.~&Please add the missing package to the manifest." (asdf::missing-requires c))))
	(format t "Downloading package ~A~%"
		(asdf::missing-requires c))
	(update-repo repo)
	(install (asdf::missing-requires c))
	(install package)))))

(defclass base-repo ()
  ((name :initarg :name :reader name)
   (additional-packages :initarg :additional-packages :initform nil))
  
  (:documentation "Base class for any distribution package")
  )

(defmethod print-object ((o base-repo) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (ignore-errors
      (prin1 (name o) stream))))

(defmethod initialize-instance :after ((p base-repo) &key &allow-other-keys) 
  (with-slots (name additional-packages) p
    (loop for package in (cons name additional-packages)
	 do
	 (setf (gethash package *all-packages*)
	       p))))

(defmethod database-dir ((p base-repo))
  (with-slots (name) p
    (merge-pathnames (make-pathname :directory `(:relative ,(string-downcase (string name))))
		     *installer-directory*)))

(defmethod working-dir ((p base-repo))
  (database-dir p))

(defun find-repo (name)
  (gethash (intern (string-upcase name) :keyword) *all-packages* nil))

(defmethod update-repo :around ((p base-repo))
  (let ((result (call-next-method p)))
    (with-slots (name additional-packages) p
      (loop for package in (cons name additional-packages)
	 do
	 (safe-shell-command nil "ln -sf ~a ~asystems/~a.asd" 
			     (asd-file p package)
			     *installer-directory* (string-downcase package))))
    result))

(defclass tarball-backed-bzr-repo (base-repo)
  ((url :initarg :url
	:documentation "the url for the gzipped tar file")
   (strip-components :initarg :strip-components :initform nil
		     :documentation "Used to strip leading directories off the files"))
)

(defmethod working-dir ((p tarball-backed-bzr-repo))
  (merge-pathnames (make-pathname :directory '(:relative "local"))
		   (database-dir p)))

(defmethod upstream-dir ((p tarball-backed-bzr-repo))
  (merge-pathnames (make-pathname :directory '(:relative "upstream"))
		   (database-dir p)))

(defmethod asd-file ((p tarball-backed-bzr-repo) &optional (package-name nil))
  (with-slots (name) p
    (car (split-sequence:split-sequence 
	  #\newline 
	  (safe-shell-command nil "find ~a -name ~a.asd | grep -v .bzr" (dir-as-file (working-dir p)) (string-downcase (or package-name name)))))))

(defmethod native-status ((p tarball-backed-bzr-repo))
  (let ((result (safe-shell-command t "cd ~a ; bzr status" (working-dir p))))
    (cond ((equalp result "")
	   nil)
	  (t
	   result))))

(defmethod local-repo-changes ((p tarball-backed-bzr-repo))
  (multiple-value-bind (result code)
      (safe-shell-command t "cd ~a ; bzr missing" (working-dir p))
    (if (eql code 0) 
	nil
	result)))

(defun replace-bzr-contents (bzr-working-dir tarball &optional (tar-options ""))
  "replace the contents of the bzr working directory with the tarball and commit any changes"
  (loop for file in (directory bzr-working-dir)
       when (not (equal (pathname-name file) ".bzr"))
     do (delete-directory-and-files file))
  (safe-shell-command nil (format nil "cd ~a ; tar xzf ~a ~a ; bzr commit -m 'new tarball received'" bzr-working-dir tar-options tarball))
  )
  
(defmethod update-repo ((p tarball-backed-bzr-repo))
  "update the local database from the cache."
  (test-environment :bzr)
	   
  (let* ((dir (database-dir p))
	 (upstream (merge-pathnames "upstream" dir)))
    (with-slots (url strip-components) p
      (makedirs (merge-pathnames "tarballs" (database-dir p)))
      (let ((tarball-path (merge-pathnames (format nil "tarballs/~a" (get-universal-time))
					   (database-dir p)))
	    (prev-tarballs (sort (mapcar #'(lambda (x) (parse-integer (pathname-name x))) (directory (merge-pathnames "tarballs/" (database-dir p)))) #'>)))
	(shttp:http-download url tarball-path)
	(cond ((eql (length prev-tarballs) 0)
	       ;; first time we grabbed a tarball, build the local repository
	       (makedirs upstream)
	       (concatenate
		'string 
		(safe-shell-command nil "cd ~a ; tar xvf ~a ~a ; bzr init"
				    upstream tarball-path
				    (if strip-components
					(format nil "--strip-components ~d" strip-components) ""))
	 
		 ;; if the default .bzrignore file is not there, add it
		 (let ((bzrpath  (make-pathname :name ".bzrignore" :defaults (upstream-dir p))))
		   (cond ((null (probe-file bzrpath))
			  (with-open-file (s bzrpath :direction :output)
			    (format s "*.fasl~&*~~~&"))))
		   (safe-shell-command nil "cd ~a ; bzr add . ; bzr commit -m 'initial tarball' ; cd .. ; bzr branch upstream local " (upstream-dir p)))))
	      ((not (files-different tarball-path (format nil "~a/tarballs/~d" (database-dir p) (car prev-tarballs))))
	       ;; no change in the tarball, leave it alone
	       (delete-file tarball-path)
	       "no new tarball")
	      (t
	       ;; new tarball. update the upstream branch
	       (replace-bzr-contents upstream tarball-path
				     (if strip-components
					 (format nil "--strip-components ~d" strip-components)))
	       ;; merge upstream changes to local branch
	       (safe-shell-command nil "cd ~a ; bzr merge " (working-dir p))))))))

(defclass cliki-repo (tarball-backed-bzr-repo)
  ()
  )

(defmethod initialize-instance :after ((p cliki-repo) &key &allow-other-keys)
  (with-slots (name url) p
    (destructuring-bind (code headers stream resolved-url)
	(shttp:http-resolve (concatenate 'string "http://www.cliki.net/" (string-downcase name) "?DOWNLOAD"))
      (setf url resolved-url)
      (if stream
	  (close stream)))))

(defclass darcs-repo (base-repo)
  ((url :initarg :url))
  )

(defmethod asd-file ((p darcs-repo) &optional (package-name nil))
  (with-slots (name) p
    (car (split-sequence:split-sequence 
	  #\newline 
	  (safe-shell-command nil "find ~a -name ~a.asd | grep -v _darcs" (dir-as-file (working-dir p)) (string-downcase (or package-name name)))))))

(defmethod native-status ((p darcs-repo))
  (let ((result (safe-shell-command t "cd ~a ; darcs whatsnew" (working-dir p))))
    (cond ((search "No changes!" result)
	   nil)
	  (t
	   result))))

(defmethod local-repo-changes ((p darcs-repo))
  (let ((result (safe-shell-command nil "cd ~a ; darcs send --dry-run" (working-dir p))))
    (cond ((search "No recorded local changes to send!" result)
	   nil)
	  (t
	   result))))

(defmethod update-repo ((p darcs-repo))
  "update the local database from the cache."

  (test-environment :darcs)
  (let* ((dir (database-dir p)))
    (with-slots (url) p
      (cond ((not (probe-file dir))
	     ;; darcs repo is not there yet, get it
	     (safe-shell-command nil "darcs get ~a ~a" url dir))
	    (t
	     (safe-shell-command nil "cd ~a ; darcs pull" dir))))))

(defclass git-repo (base-repo)
  ((url :initarg :url))
  )

(defmethod asd-file ((p git-repo) &optional (package-name nil)) 
  (with-slots (name) p
    (car (split-sequence:split-sequence 
	  #\newline 
	  (safe-shell-command nil "find ~a -name ~a.asd | grep -v .git" (dir-as-file (working-dir p)) (string-downcase (or package-name name)))))))

(defmethod native-status ((p git-repo))
  (let ((result (safe-shell-command t "cd ~a ; git status" (working-dir p))))
    (cond ((search "nothing to commit (working directory clean)" result)
	   nil)
	  (t result))))

(defmethod local-repo-changes ((p git-repo))
  (let ((result (safe-shell-command nil "cd ~a ; git branch -v" (working-dir p))))
    (cond ((search "[ahead " result)
	   result)
	  (t nil))))

(defmethod update-repo ((p git-repo))
  (test-environment :git)
  (let* ((dir (database-dir p)))
    (with-slots (url release) p
      (cond ((not (probe-file (make-pathname :name ".git" :defaults dir)))
	     (makedirs dir)
	     ;; darcs repo is not there yet, get it
	     (safe-shell-command nil"git clone ~a ~a" url dir))
	    (t
	     (safe-shell-command nil "cd ~a ; git pull" dir))))))

(defclass svn-repo (base-repo)
  ((url :initarg :url))
  )

(defmethod asd-file ((p svn-repo) &optional (package-name nil))
  (with-slots (name) p
    (car (split-sequence:split-sequence 
	  #\newline 
	  (safe-shell-command nil "find ~a -name ~a.asd | grep -v .svn" (dir-as-file (working-dir p)) (string-downcase (or package-name name)))))))

(defmethod native-status ((p svn-repo))
  (let ((result (safe-shell-command nil "cd ~a ; svn status" (working-dir p))))
    (cond ((equalp result "")
	   nil)
	  (t
	   result))))

(defmethod local-repo-changes ((p svn-repo))
  (let ((result (safe-shell-command nil "cd ~a ; svn status" (working-dir p))))
    result))

(defmethod update-repo ((p svn-repo))
  (test-environment :svn)
  (let* ((dir (database-dir p)))
    (with-slots (url) p
      (cond ((not (probe-file dir))
	     (makedirs dir)
	     (safe-shell-command nil "svn co ~a ~a" url dir))
	    (t
	     (safe-shell-command nil "cd ~a ; svn update" dir))))))


(defclass cvs-repo (base-repo)
  ((cvsroot :initarg :cvsroot)
   (module :initarg :module))
  )

(defmethod asd-file ((p cvs-repo) &optional (package-name nil))
  (with-slots (name) p
    (car (split-sequence:split-sequence 
	  #\newline 
	  (safe-shell-command nil "find ~a -name ~a.asd" (dir-as-file (working-dir p)) (string-downcase (or package-name name)))))))

(defmethod native-status ((p cvs-repo))
  (let ((result (safe-shell-command t "cd ~a ; cvs diff --brief" (working-dir p))))
    (cond ((search "differ" result)
	   result)
	  (t 
	   nil))))

(defmethod update-repo ((p cvs-repo))
  (let* ((dir (database-dir p)))
    (with-slots (cvsroot module) p
      (cond ((not (probe-file dir))
	     (makedirs dir)
	     (safe-shell-command nil "cd ~a ; cvs -z3 -d ~a co ~a" *installer-directory* cvsroot module))
	    (t
	     (safe-shell-command nil "cd ~a ; cvs update" dir))))))


(defun all-packages ()
  (let ((result nil))
    (maphash #'(lambda (key package)
		 (declare (ignore key))
		 (pushnew package result))
	     *all-packages*)
    result))


(defun dump-message (package msg)
  (princ "--------------------------------------------------------------------------------")
  (terpri)
  (princ (name package))
  (terpri)
  (princ "--------------------------------------------------------------------------------")
  (terpri)
  (princ msg)
  (terpri))

(defun update-all-packages ()
  "Grab the most recent changes from the upstream repositories."
  (loop for package in (all-packages)
       do
       (cond ((probe-file (working-dir package))
	      (dump-message package (update-repo package))))))

(defun repo-status ()
  "For all repositories, print changes that have been made to the working 
directory, but have not yet been committed to the local repository."
  (loop for package in (all-packages)
       do
       (let ((status (and (probe-file (working-dir package))
			  (native-status package))))
       (cond ((not (null status))
	      (dump-message package status))))))

(defun all-local-repo-changes ()
  "For all repositories, print changes that have been made to the working 
directory, but have not yet been committed to the local repository."
  (loop for package in (all-packages)
       do
       (let ((status (and (probe-file (working-dir package))
			  (local-repo-changes package))))
       (cond ((not (null status))
	      (dump-message package status))))))
  