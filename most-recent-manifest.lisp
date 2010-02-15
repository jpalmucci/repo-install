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
 :name :trivial-http
 :url "http://common-lisp.net/project/trivial-http")

;; add all darcs repos from http://common-lisp.net/~loliveira/ediware/
(mapcar #'(lambda (name)
	    (make-instance
	     'darcs-repo
	     :name name
	     :url (concatenate 'string
			       "http://common-lisp.net/~loliveira/ediware/"
			       (string-downcase (string name)))))

	'(:chunga
	  :cl-fad
	  :cl-gd
	  :cl-interpol
	  :cl-ppcre
	  :cl-who
	  :drakma
	  :flexi-streams
	  :html-template
	  :hunchentoot
	  :url-rewrite
	  :cl-unicode
	  :capi-overview
	  :cl-dongle
	  :cl-wbxml
	  :documentation-template
	  :fm-plugin-tools
	  :html-extract
	  :lw-add-ons
	  :lw-doc
	  :lw-win
	  :midgets
	  :odd-streams
	  :rdnzl
	  :regex-plugin
	  :cl-webdav))

(make-instance
 'darcs-repo
 :name :trivial-shell
 :url "http://common-lisp.net/project/trivial-shell")

(make-instance
 'svn-repo
 :name :usocket
 :url "svn://common-lisp.net/project/usocket/svn/usocket/trunk")

(make-instance
 'cliki-repo
 :name :split-sequence
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
 'git-repo
 :name :gsll
 :url "git://repo.or.cz/gsll.git")

(make-instance
 'cliki-repo
 :name :cl-utilities
 :strip-components 1)

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
 'git-repo
 :name :fare-csv
 :url "http://common-lisp.net/r/users/frideau/fare-csv.git")

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
 'git-repo
 :name :fare-matcher
 :url "git://common-lisp.net/users/frideau/fare-matcher.git")

(make-instance
 'git-repo
 :name :fare-utils
 :url "http://common-lisp.net/r/users/frideau/fare-utils.git")

(make-instance
 'darcs-repo
 :name :cl-json
 :url "http://common-lisp.net/project/cl-json/darcs/cl-json")

(make-instance
 'darcs-repo
 :name :postmodern
 :additional-packages '(:s-sql :cl-postgres)
 :url "http://common-lisp.net/project/postmodern/darcs/postmodern")

(make-instance
 'darcs-repo
 :name :trivial-utf-8
 :url "http://common-lisp.net/project/trivial-utf-8/darcs/trivial-utf-8")

(make-instance
 'darcs-repo
 :name :ieee-floats
 :url "http://common-lisp.net/project/ieee-floats/darcs/ieee-floats")

(make-instance
 'git-repo
 :name :clsql
 :url "git://git.b9.com/clsql.git"
 :additional-packages '(:clsql-aodbc
			:clsql-db2
			:clsql-mysql
			:clsql-odbc
			:clsql-oracle
			:clsql-postgresql-socket
			:clsql-postgresql
			:clsql-sqlite
			:clsql-sqlite3
			:clsql-tests
			:clsql-uffi
			))

(make-instance
 'git-repo
 :name :uffi
 :url "git://git.b9.com/uffi.git")


(make-instance
 'git-repo
 :name :cl-future
 :url "git://github.com/jpalmucci/cl-future.git")

(make-instance
 'cvs-repo
 :name :cl-soap
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/cl-soap/cvsroot"
 :module "cl-soap")

(make-instance
 'cvs-repo
 :name :s-xml
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/s-xml/cvsroot"
 :module "s-xml")

(make-instance
 'darcs-repo
 :name :trivial-timeout
 :url "http://common-lisp.net/project/trivial-timeout"
)

(make-instance
 'cliki-repo
 :name :salza2)

(make-instance
 'darcs-repo
 :name :cl-twitter
 :url "http://www.common-lisp.net/project/cl-twitter/darcs/cl-twitter"
 :additional-packages '(:cl-twitter-db))

(make-instance
 'tarball-backed-bzr-repo
 :name :cl-unicode
 :url "http://weitz.de/files/cl-unicode.tar.gz"
 :strip-components 1)

(make-instance
 'darcs-repo
 :name :elephant
 :url "http://www.common-lisp.net/project/elephant/darcs/elephant-1.0"
 :additional-packages '(:ele-bdb :ele-clp :ele-postmodern))

(make-instance
 'cliki-repo
 :name :pxmlutils
 :strip-components 1)

(make-instance
 'cvs-repo
 :name :portableaserve
 :cvsroot ":pserver:anonymous@portableaserve.cvs.sourceforge.net:/cvsroot/portableaserve"
 :module "portableaserve"
 :additional-packages '(:acl-compat))

(make-instance
 'git-repo
 :name :cl-mysql
 :url "http://www.hackinghat.com/repositories/cl-mysql.git/")

(make-instance
 'darcs-repo
 :name :cl-perec
 :url "http://common-lisp.net/project/cl-perec/darcs/cl-perec")

(make-instance
 'darcs-repo
 :name :cl-rdbms
 :url "http://common-lisp.net/project/cl-rdbms/darcs/cl-rdbms"
 :additional-packages '((:cl-rdbms.oracle :cl-rdbms)
                        (:cl-rdbms.postgresql :cl-rdbms)
                        (:cl-rdbms.postmodern :cl-rdbms)
                        (:cl-rdbms.sqlite :cl-rdbms)
                        (:cl-rdbms-test :cl-rdbms)))

(make-instance
 'darcs-repo
 :name :defclass-star
 :url "http://common-lisp.net/project/defclass-star/darcs/defclass-star")

(make-instance
 'darcs-repo
 :name :cl-syntax-sugar
 :url "http://common-lisp.net/project/cl-syntax-sugar/darcs/cl-syntax-sugar")

(make-instance
 'darcs-repo
 :name :cl-walker
 :url "http://common-lisp.net/project/cl-walker/darcs/cl-walker")

(make-instance
 'darcs-repo
 :name :cl-serializer
 :url "http://common-lisp.net/project/cl-serializer/darcs/cl-serializer")

(make-instance
 'darcs-repo
 :name :local-time
 :url "http://common-lisp.net/project/local-time/darcs/local-time")

(make-instance
 'cliki-repo
 :name :ironclad)

(make-instance
 'darcs-repo
 :name :cl-yalog
 :url "http://www.common-lisp.net/project/cl-dwim/darcs/cl-yalog")

(make-instance
 'darcs-repo
 :name :cl-def
 :url "http://www.common-lisp.net/project/cl-def/darcs/cl-def/")

(make-instance
 'darcs-repo
 :name :metacopy
 :url "http://www.common-lisp.net/project/cl-dwim/darcs/metacopy")

(make-instance
 'darcs-repo
 :name :contextl
 :url "http://www.common-lisp.net/project/closer/darcs/contextl")

(make-instance
 'darcs-repo
 :name :lw-compat
 :url "http://www.common-lisp.net/project/closer/darcs/lw-compat")

(make-instance
 'darcs-repo
 :name :computed-class
 :url "http://www.common-lisp.net/project/computed-class/darcs/computed-class")

(make-instance
 'darcs-repo
 :name :eager-future
 :url "http://common-lisp.net/project/eager-future/repository/eager-future")

(make-instance
 'git-repo
 :name :closure-html
 :url "git://repo.or.cz/closure-html.git")

(make-instance
 'cvs-repo
 :name :cl-plplot
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/cl-plplot/cvsroot"
 :module "cl-plplot")

(make-instance
 'git-repo
 :name :cl-2d
 :url "git://github.com/tpapp/cl-2d.git")

(make-instance
 'git-repo
 :name :cl-numlib
 :url "git://github.com/tpapp/cl-numlib.git")

(make-instance
 'git-repo
 :name :array-operations
 :url "git://github.com/tpapp/array-operations.git")

(make-instance
 'git-repo
 :name :cl-colors
 :url "git://github.com/tpapp/cl-colors.git")

(make-instance
 'git-repo
 :name :cl-cairo2
 :url "git://github.com/tpapp/cl-cairo2.git")

(make-instance
 'cvs-repo
 :name :mcclim
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/mcclim/cvsroot"
 :module "mcclim"
 :additional-packages '(:clim-examples :clim-listener :clouseau)
 )

(make-instance
 'cvs-repo
 :name :flexichain
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/flexichain/cvsroot"
 :module "flexichain")

(make-instance
 'darcs-repo
 :name :spatial-trees
 :url "http://rvw.doc.gold.ac.uk/sullivan/darcs/spatial-trees")

(make-instance
 'darcs-repo
 :name :clx
 :url "http://common-lisp.net/~crhodes/clx")

(make-instance
 'cvs-repo
 :name :cl-prevalence
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/cl-prevalence/cvsroot"
 :module "cl-prevalence")

(make-instance
 'darcs-repo
 :name :s-sysdeps
 :url "http://www.beta9.be/darcs/s-sysdeps")

(make-instance
 'git-repo
 :name :routes
 :additional-packages '(:routes.unify :routes-test)
 :url "git://github.com/archimag/cl-routes.git")
