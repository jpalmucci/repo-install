(in-package :ri)

(defparameter *installer-directory* 
  #.(parent-directory (or *compile-file-truename* *load-truename*))
  "where new packages are stored (siblings to this directory)")

(defvar *tar-command*
  #+openbsd "gtar"
  #-openbsd "tar")

(defvar *all-packages* (make-hash-table)
  "map from package name to instance")

(defun flush-repos ()
  (setq *all-packages* (make-hash-table)))
