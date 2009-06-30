(in-package :ri)

(defparameter *current-manifest* *load-truename*)

;; This manifest contains references to the newest versions of the libraries that are available.

(make-instance
 'cliki-repo
 :name :xmls
 :strip-components 1)

(make-instance
 'git-repo
 :name :repo-install
 :url "git://github.com/jpalmucci/repo-install.git")

;; this work word as a repo because there is no asd file
#+ignore
(make-instance
 'git-repo
 :name :asdf
 :url "http://common-lisp.net/project/asdf/asdf.git"
)

(make-instance
 'git-repo
 :name :osicat
 :url "http://common-lisp.net/project/osicat/git/osicat.git")

(make-instance
 'darcs-repo
 :name :cl-fad
 :url "http://common-lisp.net/~loliveira/ediware/cl-fad")

(make-instance
 'darcs-repo
 :name :trivial-http
 :url "http://common-lisp.net/project/trivial-http"
)

(make-instance
 'svn-repo
 :name :usocket
 :url "svn://common-lisp.net/project/usocket/svn/usocket/trunk")

(make-instance
 'cliki-repo
 :name :split-sequence
 :strip-components 1)

;; packages ubove this point are required for repo-installer

(make-instance
 'darcs-repo
 :name :cffi
 :additional-packages '(:cffi-grovel)
 :url "http://common-lisp.net/project/cffi/darcs/cffi"
)

(make-instance
 'darcs-repo
 :name :babel
 :url "http://common-lisp.net/project/babel/darcs/babel"
)

(make-instance
 'darcs-repo
 :name :alexandria
 :url "http://common-lisp.net/project/alexandria/darcs/alexandria"
)

(make-instance
 'darcs-repo
 :name :trivial-features
 :url "http://common-lisp.net/~loliveira/darcs/trivial-features"
)

(make-instance
 'git-repo
 :name :gsll
 :url "git://repo.or.cz/gsll.git")

(make-instance
 'cliki-repo
 :name :cl-utilities
 :strip-components 1)

(make-instance
 'darcs-repo
 :name :drakma
 :url "http://common-lisp.net/~loliveira/ediware/drakma")

(make-instance
 'darcs-repo
 :name :chunga
 :url "http://common-lisp.net/~loliveira/ediware/chunga")

(make-instance
 'darcs-repo
 :name :flexi-streams
 :url "http://common-lisp.net/~loliveira/ediware/flexi-streams")

(make-instance 
 'cliki-repo
 :name :trivial-gray-streams
 :strip-components 1)

(make-instance 
 'cliki-repo
 :name :cl-base64
 :strip-components 1)

(make-instance
 'cvs-repo
 :name :cl+ssl
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/cl-plus-ssl/cvsroot"
 :module "cl+ssl")

(make-instance
 'git-repo
 :name :puri
 :url "git://git.b9.com/puri.git")

(make-instance
 'git-repo
 :name :lispstat
 :url "git://repo.or.cz/CommonLispStat.git")

(make-instance
 'git-repo
 :name :rsm-string
 :url "http://git.debian.org/git/pkg-common-lisp/cl-rsm-string.git")

(make-instance
 'darcs-repo
 :name :lift
 :url "http://common-lisp.net/project/lift")

(make-instance
 'git-repo
 :name :lisp-matrix
 :url "git://github.com/blindglobe/lisp-matrix.git")

(make-instance
 'git-repo
 :name :ffa
 :url "git://github.com/tpapp/ffa.git")

(make-instance
 'darcs-repo
 :name :iterate
 :url "http://www.common-lisp.net/project/iterate/darcs/iterate")

(make-instance
 'darcs-repo
 :name :metabang-bind
 :url "http://common-lisp.net/project/metabang-bind")

(make-instance
 'darcs-repo
 :name :hunchentoot
 :url "http://common-lisp.net/~loliveira/ediware/hunchentoot")

(make-instance
 'darcs-repo
 :name :bordeaux-threads
 :url "http://common-lisp.net/project/bordeaux-threads/darcs/bordeaux-threads")

(make-instance 
 'cvs-repo
 :name :rfc2822
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/rfc2822/cvsroot"
 :module "rfc2822")

(make-instance 
 'cliki-repo
 :name :rfc2388
 :strip-components 1)

(make-instance 
 'cliki-repo
 :name :md5
 :strip-components 1)

;; forward declare the test package
(defpackage cl-ppcre-test)
(make-instance
 'darcs-repo
 :name :cl-ppcre
 :additional-packages '(:cl-ppcre-test)
 :url "http://common-lisp.net/~loliveira/ediware/cl-ppcre"
 :tester #'(lambda ()
	     (asdf:load-system :cl-ppcre-test)
	     (cl-ppcre-test::test)))

(make-instance
 'darcs-repo
 :name :cl-containers
 :url "http://common-lisp.net/project/cl-containers")

(make-instance
 'darcs-repo
 :name :metatilities-base
 :url "http://common-lisp.net/project/metatilities-base")

(make-instance
 'darcs-repo
 :name :metatilities
 :url "http://common-lisp.net/project/metatilities")

(make-instance
 'darcs-repo
 :name :asdf-binary-locations
 :url "http://common-lisp.net/project/asdf-binary-locations")

(make-instance
 'darcs-repo
 :name :cl-store
 :url "http://common-lisp.net/project/cl-store/darcs/cl-store")

(make-instance
 'svn-repo
 :name :cl-lex
 :url "http://cl-lex.googlecode.com/svn/trunk/")

(make-instance
 'cliki-repo
 :name :fare-csv)

(make-instance
 'cliki-repo
 :name :parse-number)
