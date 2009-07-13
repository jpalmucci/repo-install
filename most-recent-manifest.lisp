(in-package :ri)

(defparameter *current-manifest* *load-truename*)

(flush-repos)

;; forward declare some packages that we need to reference below, but that may not be loaded yet
(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (not (find-package :lift))
      (defpackage lift)))

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
 :additional-packages '(:babel-tests)
 :tester #'(lambda ()
	     (ri:install :babel-tests)
	     (parse-stefil-results #'(lambda ()
				   (asdf:oos 'asdf:test-op :babel-tests))))
)

(make-instance
 'darcs-repo
 :name :alexandria
 :url "http://common-lisp.net/project/alexandria/darcs/alexandria"
 :additional-packages '(:alexandria-tests)
 :tester #'(lambda ()
	     (ri:install  :alexandria-tests)
	     (parse-rt-results #'(lambda ()
				   (asdf:oos 'asdf:test-op :alexandria-tests))))
)

(make-instance 
 'cliki-repo
 :name :rt)

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
 :url "http://common-lisp.net/project/bordeaux-threads/darcs/bordeaux-threads"
 :tester #'(lambda ()
	     (load (make-pathname :directory `(,@(butlast (pathname-directory *current-manifest*)) "bordeaux-threads")
		   :name "bordeaux-threads-test"
		   :type "lisp"))
	     (return-lift-results
	      (lift::run-tests :suite (lift::find-testsuite "TEST-BORDEAUX-THREADS")))))

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
 #+ignore
 :tester 
 #+ignore
 #'(lambda ()
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

(make-instance
 'git-repo
 :name :closure-common
 :url "git://repo.or.cz/closure-common.git")

(make-instance
 'git-repo
 :name :cxml
 :url "git://repo.or.cz/cxml.git")

(make-instance
 'git-repo
 :name :closure-html
 :url "git://repo.or.cz/closure-html.git")

(make-instance
 'git-repo
 :name :closure-common
 :url "git://repo.or.cz/closure-common.git")


(make-instance 
 'darcs-repo
 :name :asdf-system-connections
 :url "http://common-lisp.net/project/asdf-system-connections")

(make-instance
 'darcs-repo
 :name :moptilities
 :url "http://common-lisp.net/project/moptilities")

(make-instance 
 'darcs-repo
 :name :closer-mop
 :url "http://common-lisp.net/project/closer/repos/closer-mop")

(make-instance 
 'darcs-repo
 :name :trivial-garbage
 :url "http://common-lisp.net/~loliveira/darcs/trivial-garbage")

(make-instance 
 'darcs-repo
 :name :stefil
 :url "http://common-lisp.net/project/stefil/darcs/stefil")

(make-instance
 'svn-repo
 :name :html-entities
 :url "http://html-entities.googlecode.com/svn/trunk")

(make-instance
 'mercurial-repo
 :name :weblocks
 :url "http://bitbucket.org/skypher/weblocks-stable"
)

(make-instance
 'darcs-repo
 :name :f-underscore
 :url "http://common-lisp.net/project/bpm/darcs/f-underscore")

(make-instance
 'cvs-repo
 :name :anaphora
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/anaphora/cvsroot"
 :module "src")

(make-instance
 'git-repo
 :name :parenscript
 :url "http://common-lisp.net/project/parenscript/git/parenscript")

(make-instance
 'darcs-repo
 :name :cl-cont
 :url "http://common-lisp.net/project/cl-cont/darcs/cl-cont")

(make-instance
 'cliki-repo
 :name :fare-matcher)

(make-instance
 'cliki-repo
 :name :fare-utils)

(make-instance 
 'darcs-repo
 :name :cl-json
 :url "http://common-lisp.net/project/cl-json/darcs/cl-json")

(make-instance 
 'darcs-repo
 :name :cl-who
 :url "http://common-lisp.net/~loliveira/ediware/cl-who")
