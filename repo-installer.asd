(in-package :asdf)

(defpackage repo-installer
  (:nicknames ri)
  (:use :cl)
  (:export #:install #:update-all-packages #:repo-status)
)

(asdf:defsystem repo-installer
  :depends-on (trivial-http)
  :components ((:file "utilities")
	       (:file "vars")
	       (:file "installer"))
  :serial t)
