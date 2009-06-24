(in-package :asdf)

(defpackage repo-install
  (:nicknames ri)
  (:use :cl)
  (:export
   #:install
   #:find-repo
   #:update-repo
   #:update-all-repos
   #:repo-status
   #:all-repo-status
   #:*current-manifest*)
)

(asdf:defsystem repo-install
  :depends-on (trivial-http cl-fad)
  :components ((:file "utilities")
	       (:file "vars")
	       (:file "installer"))
  :serial t)
