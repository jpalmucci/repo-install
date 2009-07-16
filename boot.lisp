
;; load the old version of asdf so we won't get an undefined function error
;; when sbcl tries to use ASDF::MODULE-PROVIDE-ASDF, which doesn't seem to be
;; in the newest release of asdf
#+sbcl
(require :asdf)

(load (merge-pathnames (make-pathname :directory '(:relative ".." "asdf") :name "asdf")
		       *load-truename*))

(pushnew (make-pathname :directory (append (butlast (pathname-directory *load-truename*)) '("systems")))
	 asdf:*central-registry*)

(asdf:load-system :repo-install)

(load (merge-pathnames (make-pathname :name "most-recent-manifest") *load-truename*))
