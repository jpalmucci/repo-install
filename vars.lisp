(in-package :ri)

(defparameter *installer-directory* 
  (parent-directory *load-pathname*)
  "where new packages are stored (siblings to this directory)")

(pushnew *installer-directory* asdf:*central-registry*)

(defvar *all-packages* (make-hash-table)
  "map from package name to instance")

(defun flush-repos ()
  (setq *all-packages* (make-hash-table)))