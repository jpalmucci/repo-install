(in-package :ri)

(defparameter *installer-directory*
  (parent-directory *load-truename*)
  "where new packages are stored (siblings to this directory)")

(defvar *all-packages* (make-hash-table)
  "map from package name to instance")

(defun flush-repos ()
  (setq *all-packages* (make-hash-table)))