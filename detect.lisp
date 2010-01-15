(in-package :ri)

(defun is-managed (package)
  "Returns t if the package is installed and managed by the
   current repo-install"
  (cl-fad:directory-exists-p (database-dir package)))

(defun all-managed-packages ()
  "Returns all installed packages managed by the current repo-install"
  (let ((result nil))
    (maphash #'(lambda (key package)
                 (declare (ignore key))
                 (if (is-managed package)
                     (pushnew package result)))
             *all-packages*)
    result))

(defun pathname-dirname (path)
  (car (last (pathname-directory path))))

(defun detect-scm (dir)
  "Detect if DIR is a managed source code repository"
  (loop
     for (scm . subdir) in '((bazaar-repo     . ".bzr")
                             (cvs-repo        . "CVS")
                             (darcs-repo      . "_darcs")
                             (git-repo        . ".git")
                             (mercurial-repo  . ".hg")
                             (svn-repo        . ".svn"))
     if (cl-fad:directory-exists-p (merge-pathnames subdir dir))
     collect scm into results
     finally (return (if (cdr results)
                         results
                         (car results)))))

(defun regex-command (regex &rest command)
  (cl-ppcre:do-register-groups
      (first) (regex (apply 'safe-shell-command t command))
    (return-from regex-command first)))

(defun detect-initargs (dir repo-type)
  (case repo-type
    ((cvs-repo)
     (list :cvsroot (regex-command "^(.*)$" "(cat ~a/CVS/Root)" dir)
           :module (regex-command "^(.*)$" "(cat ~a/CVS/Repository)" dir)))
    ((darcs-repo)
     (list :url (regex-command "Default Remote: (.*)"
                               "(cd ~a ; darcs show repo)" dir)))
    ((git-repo)
     (list :url (regex-command "^(.*)$"
                               "(cd ~a ; git config --get remote.origin.url)"
                               dir)))
    ((mercurial-repo) ;; FIXME
     (list :url (regex-command "^Pull: (.*)$"
                               "(cd ~a ; hg summary --remote)" dir)))
    ((svn-repo)
     (list :url (regex-command "URL.: (.*)" "(cd ~a ; svn info)" dir)))))

(defun detect-repo (dir)
  (when (cl-fad:directory-exists-p dir)
    (let* ((name (pathname-dirname dir))
           (asd (probe-file (make-pathname :name name :type "asd"
                                           :defaults dir)))
           (scm (detect-scm dir))
           (initargs (detect-initargs dir scm)))
      (if (and asd scm initargs)
          `(:type ,scm :name ,(intern (string-upcase name) :keyword)
                  ,@initargs)))))

(defun detect-all-repos ()
  (loop
     for dir in (directory (make-pathname :name :wild
                                          :defaults *installer-directory*))
     for d = (or (find-repo (pathname-dirname dir))
                 (detect-repo dir))
     if (consp d)
     collect d))

(defun repo-definition (d &optional stream)
  (let ((*print-case* :downcase))
    (format stream "(make-instance~% '~a~{~% ~S ~S~})~%"
            (getf d :type)
            (nthcdr 2 d))))

(defun show-detected-repos (&optional (stream t))
  "Print definitions for all detected repositories not in manifest."
  (dolist (d (detect-all-repos))
    (format stream "~%")
    (repo-definition d stream)))

(defun add-detected-repos ()
  "Add detected repositories to current manifest
   without changing most-recent-manifest.lisp ."
  (dolist (d (detect-all-repos))
    (format t "~%")
    (repo-definition d t)
    (apply 'make-instance
           (getf d :type)
           (nthcdr 2 d))))
