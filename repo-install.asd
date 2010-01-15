(in-package :asdf)

(defpackage repo-install
  (:nicknames ri)
  (:use :cl)
  (:export
   #:install
   #:find-repo
   #:update-repo
   #:test-all-repos
   #:update-all-repos
   #:repo-status
   #:all-repo-status
   #:local-repo-changes
   #:all-local-repo-changes
   #:is-managed
   #:all-managed-packages
   #:show-detected-repos
   #:add-detected-repos
   #:*current-manifest*)
)

(asdf:defsystem repo-install
  :depends-on (cl-fad cl-ppcre trivial-shell)
  :components ((:file "utilities")
	       (:file "vars")
	       (:file "installer")
	       (:file "detect")
	       (:static-file "most-recent-manifest.lisp"))
  :serial t)
