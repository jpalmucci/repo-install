(in-package :ri)

(defparameter *installer-directory* 
  (parent-directory *load-pathname*)
  "where new packages are stored (siblings to this directory)")

(pushnew *installer-directory* asdf:*central-registry*)

(defparameter *git-location*
  (cond ((probe-file "/opt/local/bin/git"))
	((probe-file "/usr/local/bin/git"))
	((probe-file "/usr/bin/git")))
  "Location of the git executable. Nil if not installed")

(defparameter *darcs-location*
  (cond ((probe-file "/opt/local/bin/darcs"))
	((probe-file "/usr/local/bin/darcs"))
	((probe-file "/usr/bin/darcs")))
  "Location of the darcs executable. Nil if not installed")

(defparameter *svn-location*
  (cond ((probe-file "/opt/local/bin/svn"))
	((probe-file "/usr/local/bin/svn"))
	((probe-file "/usr/bin/svn")))
  "Location of the svn executable. Nil if not installed")

(defparameter *bzr-location*
  (cond ((probe-file "/opt/local/bin/bzr"))
	((probe-file "/usr/local/bin/bzr"))
	((probe-file "/usr/bin/bzr")))
  "Location of the bzr executable. Nil if not installed")

(defvar *all-packages* (make-hash-table)
  "map from package name to instance")

