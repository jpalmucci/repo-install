
(load (merge-pathnames (make-pathname :directory '(:relative "asdf") :name "asdf")
		       *load-truename*))

(pushnew (merge-pathnames (make-pathname :directory '(:relative "systems"))
		       *load-truename*) asdf:*central-registry*)

(asdf:load-system :repo-installer)

(load (merge-pathnames (make-pathname :directory '(:relative "repo-installer") :name "unstable-manifest")
		       *load-truename*))
