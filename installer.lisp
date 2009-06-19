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
      (let ((repo (find-dpackage (asdf::missing-requires c))))
	(cond ((null repo)
	       (error "Don't know how to get the ~a package" (asdf::missing-requires c))))
	(format t "Downloading package ~A~%"
		(asdf::missing-requires c))
	(update-database repo)
	(install (asdf::missing-requires c))
	(install package)))))

(defclass base-repo ()
  ((name :initarg :name :reader name)
   (additional-packages :initarg :additional-packages :initform nil))
  
  (:documentation "Base class for any distribution package")
  )

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

(defun find-dpackage (name)
  (gethash (intern (string-upcase name) :keyword) *all-packages* nil))

(defmethod update-database :around ((p base-repo))
  (call-next-method p)
  (with-slots (name additional-packages) p
    (loop for package in (cons name additional-packages)
       do
       (safe-shell-command nil "ln -sf ~a ~asystems/~a.asd" 
			   (asd-file p package)
			   *installer-directory* (string-downcase package)))))

(defclass tarball-backed-bzr-repo (base-repo)
  ((url :initarg :url
	:documentation "the url for the gzipped tar file")
   (strip-components :initarg :strip-components :initform nil
		     :documentation "Used to strip leading directories off the files"))
)

(defmethod working-dir ((p tarball-backed-bzr-repo))
  (merge-pathnames (make-pathname :directory '(:relative "local"))
		   (database-dir p)))

(defmethod asd-file ((p tarball-backed-bzr-repo) &optional (package-name nil))
  (with-slots (name) p
    (car (split-sequence:split-sequence 
	  #\newline 
	  (safe-shell-command nil "find ~a -name ~a.asd | grep -v .bzr" (dir-as-file (working-dir p)) (string-downcase (or package-name name)))))))

(defmethod native-status ((p tarball-backed-bzr-repo))
  (safe-shell-command t "cd ~a ; bzr status" (working-dir p)))

(defun replace-bzr-contents (bzr-working-dir tarball &optional (tar-options ""))
  "replace the contents of the bzr working directory with the tarball and commit any changes"
  (loop for file in (directory bzr-working-dir)
       when (not (equal (pathname-name file) ".bzr"))
     do (delete-directory-and-files file))
  (safe-shell-command nil (format nil "cd ~a ; tar xzf ~a ~a ; bzr commit -m 'new tarball received'" bzr-working-dir tar-options tarball))
  )
  
(defmethod update-database ((p tarball-backed-bzr-repo))
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
	       (safe-shell-command nil "cd ~aupstream ; tar xvf ~a ~a ; cd upstream ; bzr init ; bzr add . ; bzr commit -m 'initial tarball' ; cd .. ; bzr branch upstream local " 
			    dir tarball-path
			    (if strip-components
				(format nil "--strip-components ~d" strip-components) ""))
	       (safe-shell-command nil "ln -s -f `find ~alocal -name '*.asd'` ~asystems" dir *installer-directory*)
	       'new-package)
	      ((equalp (excl:file-contents tarball-path)
		       (excl:file-contents (format nil "~a/tarballs/~d" (database-dir p) (car prev-tarballs))))
	       ;; no change in the tarball, leave it alone
	       (delete-file tarball-path)
	       'no-change)
	      (t
	       ;; new tarball. update the upstream branch
	       (replace-bzr-contents upstream tarball-path
				     (if strip-components
					 (format nil "--strip-components ~d" strip-components)))
	       ;; merge upstream changes to local branch
	       (safe-shell-command nil "cd ~a/local ; bzr merge " dir-path)
	       'new-tarball))))))

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
  (safe-shell-command t "cd ~a ; darcs whatsnew" (working-dir p)))

(defmethod update-database ((p darcs-repo))
  "update the local database from the cache."

  (test-environment :darcs)
  (let* ((dir (database-dir p)))
    (with-slots (url) p
      (cond ((not (probe-file dir))
	     ;; darcs repo is not there yet, get it
	     (safe-shell-command nil "darcs get ~a ~a" url dir)
	     'new-package)
	    (t
	     (safe-shell-command nil "cd ~a ; darcs pull" dir)
	     'pulled)))))

(defclass git-repo (base-repo)
  ((url :initarg :url))
  )

(defmethod asd-file ((p git-repo) &optional (package-name nil)) 
  (with-slots (name) p
    (car (split-sequence:split-sequence 
	  #\newline 
	  (safe-shell-command nil "find ~a -name ~a.asd | grep -v .git" (dir-as-file (working-dir p)) (string-downcase (or package-name name)))))))

(defmethod native-status ((p git-repo))
  (safe-shell-command t "cd ~a ; git status" (working-dir p)))

(defmethod update-database ((p git-repo))
  (test-environment :git)
  (let* ((dir (database-dir p)))
    (with-slots (url release) p
      (cond ((not (probe-file (make-pathname :name ".git" :defaults dir)))
	     (makedirs dir)
	     ;; darcs repo is not there yet, get it
	     (safe-shell-command nil"git clone ~a ~a" url dir)
	     'new-package)
	    (t
	     (safe-shell-command nil "cd ~a ; git pull" dir)
	     'pulled)))))

(defclass svn-package (base-repo)
  ((url :initarg :url))
  )

(defmethod asd-file ((p svn-package) &optional (package-name nil))
  (with-slots (name) p
    (car (split-sequence:split-sequence 
	  #\newline 
	  (safe-shell-command nil "find ~a -name ~a.asd | grep -v .svn" (dir-as-file (working-dir p)) (string-downcase (or package-name name)))))))

(defmethod native-status ((p svn-package))
  (safe-shell-command t "cd ~a ; svn status" (working-dir p)))

(defmethod update-database ((p svn-package))
  (test-environment :svn)
  (let* ((dir (database-dir p)))
    (with-slots (url) p
      (cond ((not (probe-file dir))
	     (makedirs dir)
	     (safe-shell-command nil "svn co ~a ~a" url dir)
	     'new-package)
	    (t
	     (safe-shell-command nil "cd ~a ; svn update" dir)
	     'pulled)))))


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
  (safe-shell-command t "cd ~a ; cvs foo" (working-dir p)))

(defmethod update-database ((p cvs-repo))
  (let* ((dir (database-dir p)))
    (with-slots (cvsroot module) p
      (cond ((not (probe-file dir))
	     (makedirs dir)
	     (safe-shell-command nil "cd ~a ; cvs -z3 -d ~a co ~a" *installer-directory* cvsroot module)
	     'new-package)
	    (t
	     (safe-shell-command nil "cd ~a ; cvs update" dir)
	     'pulled)))))


(defun all-packages ()
  (let ((result nil))
    (maphash #'(lambda (key package)
		 (pushnew package result))
	     *all-packages*)
    result))

(defun update-all-packages ()
  (loop for package in (all-packages)
       do
       (print (update-database package))))

(defun repo-status ()
  (loop for package in (all-packages)
       do
       (print (name package))
       (print (native-status package))))
  