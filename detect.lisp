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
