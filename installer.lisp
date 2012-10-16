(in-package :ri)

(defun test-environment (repo-type)
  (ecase repo-type
    (:wget
     (multiple-value-bind (result code)
	 (safe-shell-command t "which wget")
       (if (not (eql code 0))
	   (error "wget executable not found. Please install the wget url downloader try again."))))
    (:git
     (multiple-value-bind (result code)
	 (safe-shell-command t "which git")
       (if (not (eql code 0))
	   (error "git executable not found. Please install the Git version control system and try again."))))
    (:mercurial
     (multiple-value-bind (result code)
	 (safe-shell-command t "which hg")
       (if (not (eql code 0))
	   (error "hg (mercurial) executable not found. Please install the Mercurial version control system and try again."))))
    (:darcs
     (multiple-value-bind (result code)
	 (safe-shell-command t "which darcs")
       (if (not (eql code 0))
	   (error "darcs executable not found. Please install the Darcs version control system and try again."))))
    (:svn
     (multiple-value-bind (result code)
	 (safe-shell-command t "which svn")
       (if (not (eql code 0))
	   (error "svn executable not found. Please install the Subversion version control system and try again."))))
    (:bzr
     (multiple-value-bind (result code)
	 (safe-shell-command t "which bzr")
       (declare (ignore result))
       (if (not (eql code 0))
	   (error "bzr executable not found. Please install the Bazaar version control system and try again."))))))

(defmacro with-repo-install (&body body)
  "
Run BODY (possibly multiple times) and install missing packages until
successful. If a particular dependency is not found in the
*current-manifest*, signal an error. In this case, you'll have to add
the package to the manifest and try again.
"
  (let ((c (gensym "C-"))
	(tag (gensym "TAG-")))
    `(tagbody
	,tag
	(handler-bind ((asdf:missing-component (lambda (,c)
						 (handle-missing-component ,c)
						 (go ,tag))))
	  ,@body))))

(defun handle-missing-component (c)
  (let ((repo (find-repo (asdf::missing-requires c))))
    (cond ((null repo)
	   (error "Don't know how to get the ~a package.~&Please add the missing package to the manifest ~A." (asdf::missing-requires c) *current-manifest*))
	  ((probe-file (working-dir repo))
	   (error "Repository for ~A exists, but asdf could not load it."
		  (string-downcase (name repo)))))
    (format t "Downloading package ~A~%" (asdf::missing-requires c))
    (install (asdf::missing-requires c) :force t)))

(defun install (package &key (force nil))
  "
Ensure the given package and its dependencies are installed.
Use FORCE to fetch/update even if the package is installed.
"
  (if force
      (update-repo (find-repo package)))
  (with-repo-install
    (asdf:operate 'asdf:load-op package)))

(defclass base-repo ()
  ((name :initarg :name :reader name)
   (additional-packages :initarg :additional-packages :initform nil
                        :documentation "A list of other defsystems
                        that are shipped with this one. Can be a list,
                        in which case the second element is the
                        defsystem in whose asd file the defsystem is
                        defined. (common in cases where system
                        includes plugins)")
   (tester :initarg :tester :initform nil
	   :documentation "A funcallable object that will test the library. Returns true on success."))

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
         (if (listp package)
             (setq package (first package)))
	 (setf (gethash package *all-packages*)
	       p))))

(defmethod database-dir ((p base-repo))
  (with-slots (name) p
    (merge-pathnames (make-pathname :directory `(:relative ,(string-downcase (string name))))
		     *installer-directory*)))

(defmethod working-dir ((p base-repo))
  (database-dir p))

(defun find-repo (name)
  "Find the repository with the given name."
  (gethash (intern (string-upcase name) :keyword) *all-packages* nil))

(defmethod asd-file ((p base-repo) &key (package-name nil) (test nil))
  (cond ((probe-file (working-dir p))
	 (with-slots (name) p
	   (let ((name-to-look-for (symbol-name (or package-name name))))
	     (cl-fad:walk-directory
	      (working-dir p)
	      #'(lambda (x)
		  (cond ((and (or (string-equal (pathname-type x) "asd")
				  (string-equal (pathname-type x) "asdf"))
			      (string-equal (pathname-name x) name-to-look-for)
			      (or (null test)
				  (funcall test x)))
			 (return-from asd-file x)))))))))
  nil)

(defclass tarball-backed-bzr-repo (base-repo)
  ((url :initarg :url
	:documentation "The url from which to grab the gzipped tar file.")
   (strip-components :initarg :strip-components :initform nil
		     :documentation "Used to strip leading directories
		     off the files. This is necessary when the tarball
		     includes a top-level directory that contains a
		     version string. The reason we want to strip this
		     top level name is so that the local bzr
		     repository can match up the old and new versions
		     of the files"))
  (:documentation "Upstream is a set of tarballs. Local patches are maintained in a bzr repository.")
)

(defmethod working-dir ((p tarball-backed-bzr-repo))
  (merge-pathnames (make-pathname :directory '(:relative "local"))
		   (database-dir p)))

(defmethod upstream-dir ((p tarball-backed-bzr-repo))
  (merge-pathnames (make-pathname :directory '(:relative "upstream"))
		   (database-dir p)))

(defmethod asd-file ((p tarball-backed-bzr-repo) &key (package-name nil))
  (call-next-method p :package-name package-name
		    :test #'(lambda (x)
			      (not (member ".bzr" (pathname-directory x) :test #'equalp)))))

(defmethod repo-status ((p tarball-backed-bzr-repo))
  (let ((result (safe-shell-command t "(cd ~a && bzr status)" (working-dir p))))
    (cond ((equalp result "")
	   nil)
	  (t
	   result))))

(defmethod local-repo-changes ((p tarball-backed-bzr-repo))
  (multiple-value-bind (result code)
      (safe-shell-command t "(cd ~a && bzr missing)" (working-dir p))
    (if (eql code 0)
	nil
	result)))

(defun replace-bzr-contents (bzr-working-dir tarball &optional tar-options)
  "replace the contents of the bzr working directory with the tarball and commit any changes"
  (loop for file in (cl-fad:list-directory bzr-working-dir)
       when (not (equal (pathname-name (cl-fad:pathname-as-file file)) ".bzr"))
     do (if (cl-fad:directory-pathname-p file)
	    (cl-fad:delete-directory-and-files file)
	    (delete-file file)))
  (safe-shell-command nil (format nil "(cd ~a && ~a xzf ~a ~@[~a ~]&& bzr commit --unchanged -m 'new tarball received')"
				  bzr-working-dir
				  *tar-command* tarball tar-options)))

(defmethod update-repo ((s symbol))
  (with-simple-restart (abort "Skip updating ~A" s)
    (loop while
	 (nth-value 2 (with-simple-restart (retry "Retry updating ~A" s)
			(return-from update-repo
			  (update-repo (find-repo s))))))))

(defun download (url path)
  #-openbsd (test-environment :wget)
  (dump-message :message (format nil "Downloading ~A into ~A" url path))
  #+openbsd (safe-shell-command t "ftp -o ~A ~A" path url)
  #-openbsd (safe-shell-command t "wget ~a -O ~a" url path))

(defmethod update-repo ((p tarball-backed-bzr-repo))
  "update the local database from the cache."
  (test-environment :bzr)
  (let* ((dir (database-dir p))
	 (upstream (merge-pathnames "upstream" dir)))
    (with-slots (url strip-components) p
      (makedirs (merge-pathnames "tarballs" (database-dir p)))
      (let ((tarball-path (merge-pathnames (format nil "tarballs/~a" (get-universal-time))
					   (database-dir p)))
	    (prev-tarballs (sort (mapcar #'(lambda (x) (parse-integer (pathname-name x))) (cl-fad:list-directory (merge-pathnames "tarballs/" (database-dir p)))) #'>)))
	(download url tarball-path)
	(cond ((not (probe-file tarball-path))
	       (error "Couldn't download tarball"))
	      ((eql (length prev-tarballs) 0)
	       ;; make sure that we don't leave around a partially created repo
	       (delete-dir-on-error dir
		 ;; first time we grabbed a tarball, build the local repository
		 (makedirs upstream)
		 (concatenate
		  'string
		  (safe-shell-command nil "(cd ~a && ~a xzf ~a && bzr init)"
				      upstream
				      *tar-command*
				      tarball-path)

		  (safe-shell-command nil "(cd ~a && bzr add . && bzr commit -m 'initial tarball' && cd .. && bzr branch upstream local)" (upstream-dir p))
		  ;; if the default .bzrignore file is not there, add it
		  (let ((bzrpath  (make-pathname :name ".bzrignore" :defaults (working-dir p))))
		    (cond ((null (probe-file bzrpath))
			   (with-open-file (s bzrpath :direction :output)
			     (format s "*.fasl~&*~~~&"))
			   (safe-shell-command nil "(cd ~a && bzr add .bzrignore && bzr commit -m 'add default .bzrignore')" (working-dir p))
			   ))
		    ))))
	      ((not (files-different tarball-path (format nil "~a/tarballs/~d" (database-dir p) (car prev-tarballs))))
	       ;; no change in the tarball, leave it alone
	       (delete-file tarball-path)
	       nil)
	      (t
	       ;; new tarball. update the upstream branch
	       (replace-bzr-contents upstream tarball-path
				     (if strip-components
					 (format nil "--strip-components ~d" strip-components)))
	       ;; merge upstream changes to local branch
	       (safe-shell-command nil "(cd ~a && bzr merge)" (working-dir p))))))))

(defclass cliki-repo (tarball-backed-bzr-repo)
  ()
  )

(defmethod initialize-instance :after ((p cliki-repo) &key &allow-other-keys)
  (with-slots (name url) p
    (setf url (concatenate 'string "http://www.cliki.net/" (string-downcase name) "?download"))))

(defclass darcs-repo (base-repo)
  ((url :initarg :url))
  )

(defmethod asd-file ((p darcs-repo) &key (package-name nil))
  (call-next-method p :package-name package-name
		    :test #'(lambda (x)
			      (not (member "_darcs" (pathname-directory x) :test #'equalp)))))

(defmethod repo-status ((p darcs-repo))
  (let ((result (safe-shell-command t "(cd ~a && darcs whatsnew)" (working-dir p))))
    (cond ((search "No changes!" result)
	   nil)
	  (t
	   result))))

(defmethod local-repo-changes ((p darcs-repo))
  (let ((result (safe-shell-command nil "(cd ~a && darcs send --dry-run)" (working-dir p))))
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
	     ;; make sure that we don't leave around a partially created repo
	     (delete-dir-on-error dir
	       (safe-shell-command nil "darcs get ~a ~a" url dir)))
	    (t
	     (let ((result (safe-shell-command nil "(cd ~a && darcs pull -a ~a)" dir url)))
	       (cond ((search "No remote changes to pull in!" result)
		      nil)
		     (t result))))))))

(defclass git-repo (base-repo)
  ((url :initarg :url))
  )

(defmethod asd-file ((p git-repo) &key (package-name nil))
  (call-next-method p :package-name package-name
		    :test #'(lambda (x)
			      (not (member ".git" (pathname-directory x) :test #'equalp)))))

(defmethod repo-status ((p git-repo))
  (let ((result (safe-shell-command t "(cd ~a && git status)" (working-dir p))))
    (cond ((search "nothing to commit (working directory clean)" result)
	   nil)
	  (t result))))

(defmethod local-repo-changes ((p git-repo))
  (let ((result (safe-shell-command nil "(cd ~a && git branch -v)" (working-dir p))))
    (cond ((search "[ahead " result)
	   result)
	  (t nil))))

(defmethod update-repo ((p git-repo))
  (restart-bind
      ((retry (lambda () (update-repo p))
	 :report-function (lambda (s) (format s "Retry updating ~a." p))))
    (test-environment :git)
    (let* ((dir (database-dir p)))
      (with-slots (url release) p
	(cond ((not (probe-file (make-pathname :name ".git" :defaults dir)))
	       (cl-fad:delete-directory-and-files dir :if-does-not-exist :ignore)
	       ;; make sure that we don't leave around a partially created repo
	       (delete-dir-on-error dir
		 ;; repo is not there yet, get it
		 (safe-shell-command nil"git clone ~a ~a" url
				     (string-right-trim "/" (format nil "~A" dir)))))
	      (t
	       (let ((result (safe-shell-command nil "(cd ~a && git pull)" dir)))
		 (cond ((search "Already up-to-date." result)
			nil)
		       (t result)))))))))

;; ********************************************************************************

(defclass mercurial-repo (base-repo)
  ((url :initarg :url))
  )

(defmethod asd-file ((p mercurial-repo) &key (package-name nil))
  (call-next-method p :package-name package-name
		    :test #'(lambda (x)
			      (not (member ".hg" (pathname-directory x) :test #'equalp)))))

(defmethod repo-status ((p mercurial-repo))
  (let ((result (safe-shell-command t "(cd ~a && hg status)" (working-dir p))))
    (cond ((search "nothing to commit (working directory clean)" result)
	   nil)
	  (t result))))

(defmethod local-repo-changes ((p mercurial-repo))
  (let ((result (safe-shell-command t "(cd ~a && hg outgoing)" (working-dir p))))
    (cond ((search "no changes found" result)
	   nil)
	  (t result))))

(defmethod update-repo ((p mercurial-repo))
  (test-environment :mercurial)
  (let* ((dir (database-dir p)))
    (with-slots (url release) p
      (cond ((not (probe-file (make-pathname :name ".hg" :defaults dir)))
	     (cl-fad:delete-directory-and-files dir :if-does-not-exist :ignore)
	     ;; make sure that we don't leave around a partially created repo
	     (delete-dir-on-error dir
	       ;; repo is not there yet, get it
	       (safe-shell-command nil "hg clone ~a ~a" url
				   (string-right-trim "/" (format nil "~A" dir)))))
	    (t
	     (let ((result (safe-shell-command nil "(cd ~a && hg pull && hg update)" dir)))
	       (cond ((search "no changes found" result)
		      nil)
		     (t result))))))))

;; ********************************************************************************

(defclass svn-repo (base-repo)
  ((url :initarg :url))
  )

(defmethod asd-file ((p svn-repo) &key (package-name nil))
  (call-next-method p :package-name package-name
		    :test #'(lambda (x)
			      (not (member ".svn" (pathname-directory x) :test #'equalp)))))

(defmethod repo-status ((p svn-repo))
  (let ((result (safe-shell-command nil "(cd ~a && svn status)" (working-dir p))))
    (cond ((equalp result "")
	   nil)
	  (t
	   result))))

(defmethod local-repo-changes ((p svn-repo))
  (repo-status p))

(defmethod update-repo ((p svn-repo))
  (test-environment :svn)
  (let* ((dir (database-dir p)))
    (with-slots (url) p
      (cond ((not (probe-file dir))
	     ;; make sure that we don't leave around a partially created repo
	     (delete-dir-on-error dir
	       (makedirs dir)
	       (safe-shell-command nil "svn co ~a ~a" url dir)))
	    (t
	     (let ((result (safe-shell-command nil "(cd ~a && svn update)" dir)))
	       (cond ((search "At revision" result)
		      nil)
		     (t result))))))))


(defclass cvs-repo (base-repo)
  ((cvsroot :initarg :cvsroot)
   (module :initarg :module))
  )

(defmethod repo-status ((p cvs-repo))
  (let ((result (safe-shell-command t "(cd ~a && cvs diff --brief)" (working-dir p))))
    (cond ((search "differ" result)
	   result)
	  (t
	   nil))))

(defmethod local-repo-changes ((p cvs-repo))
  (repo-status p))

(defmethod update-repo ((p cvs-repo))
  (let* ((dir (database-dir p)))
    (with-slots (cvsroot module) p
      (cond ((not (probe-file dir))
	     ;; make sure that we don't leave around a partially created repo
	     (delete-dir-on-error dir
	       (makedirs dir)
	       (safe-shell-command nil "(cd ~a && cvs -z3 -d ~a co ~a)" dir cvsroot module)))
	    (t
	     (safe-shell-command nil "(cd ~a/~a && cvs update)" dir module))))))


(defun all-packages ()
  (let ((result nil))
    (maphash #'(lambda (key package)
		 (declare (ignore key))
		 (pushnew package result))
	     *all-packages*)
    result))


(defun dump-message (&key package message)
  (when package
    (fresh-line)
    (princ "-------------------------------------------------------------------------------")
    (terpri)
    (format t "~A - ~A" (name package) (class-of package))
    (terpri)
    (princ "-------------------------------------------------------------------------------")
    (terpri))
  (when message
    (fresh-line)
    (princ message)
    (terpri))
  (finish-output))

(defun update-all-repos ()
  "Grab the most recent changes from the upstream repositories."
  (maphash #'(lambda (key package)
	       (cond ((probe-file (working-dir package))
		      (dump-message :package package
				    :message "checking for updates")
		      (let ((message (update-repo key)))
			(if message
			    (dump-message :message message))))))
	   *all-packages*))

(defun all-repo-status ()
  "For all repositories, print changes that have been made to the working
directory, but have not yet been committed to the local repository."
  (loop for package in (all-packages)
       do
       (let ((status (and (probe-file (working-dir package))
			  (repo-status package))))
       (cond ((not (null status))
	      (dump-message :package package :message status))))))

(defun all-local-repo-changes ()
  "For all repositories, print changes that have been committed to the local
repository, but have not yet been committed upstream"
  (loop for package in (all-packages)
       do
       (let ((status (and (probe-file (working-dir package))
			  (local-repo-changes package))))
       (cond ((not (null status))
	      (dump-message package status))))))
