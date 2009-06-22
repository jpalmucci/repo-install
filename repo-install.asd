(in-package :asdf)

(defpackage repo-install
  (:nicknames ri)
  (:use :cl)
  (:export
   #:install
   #:find-repo
   #:update-repo
   #:update-all-packages #:repo-status #:all-local-repo-changes)
)

(asdf:defsystem repo-install
  :depends-on (trivial-http cl-fad)
  :components ((:file "utilities")
	       (:file "vars")
	       (:file "installer"))
  :serial t)
