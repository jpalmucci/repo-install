
(load (merge-pathnames (make-pathname :directory '(:relative ".." "asdf") :name "asdf")
		       *load-truename*))

(pushnew (make-pathname :directory (append (butlast (pathname-directory *load-truename*)) '("systems")))
	 asdf:*central-registry*)

(asdf:load-system :repo-install)

(load (merge-pathnames (make-pathname :name "most-recent-manifest") *load-truename*))
