Repo-install: An ASDF-install replacement geared towards collaboration
======================================================================

Repo-install: An ASDF-install replacement geared towards collaboration
----------------------------------------------------------------------

Repo-install is a common lisp package manager that can be used to
chase down and download dependencies between common lisp
libraries. Unlike other package managers (asdf-install, mudballs),
repo-install maintains the connection between source code on your disk
and the upstream repository.

In cases where the upstream library is stored using distributed
version control (DVC), the local files are stored in the same DVC
system as a clone.

When the upstream library is not a DVC, repo-install will
create a local DVC repository with the non-DVC distribution system
(tarballs, cvs, or svn) as input to the local repository.

There are two primary advantages to this: 

* Changes you make to your local copy of the library remain in place
  even when you update to a newer version of the library.

* It is much easier to submit patches to the upstream maintainer. For
  DVC, it is also much easier for the upstream maintainer to integrate
  your patches.

Both of these capabilities are particularly important when working on
common lisp. Several very useful common lisp libraries are maintained
on a less than full time basis. You may need to maintain your local
patches for weeks or forever if the upstream maintainer doesn't have
the time to integrate them sooner.

Unlike Perl, Python, and Ruby, common lisp does not have a
reference implementation. Downstream users can submit valuable
patches for different lisp environments, especially environments
that are not easily available to a library maintainer.

Finally, common-lisp users tend to be very technical. With such an
experienced user base, downstream patches should turn out to be more
valuable than not.

Why not clbuild?
----------------

[Clbuild](http://common-lisp.net/project/clbuild) is another common
lisp package manager that works with downstream repositories as
opposed to tarballs. I decided not to use it for a few reasons:

* It was missing some functionality I needed (ri:all-repo-status,
  ri:all-repo-local-changes and TODOs below).  I'm finding that this
  functionality is much easier to implement in Common Lisp.

* My emacs environment is set up to handle more than just common
  lisp. I wanted to have my package manager work within my existing
  environment.

Installing
----------

Repo-install is bootstrapped using a tarball that can be found
[here](http://www.machineinsight.com/repo-install/repo-install-bootstrap.tgz).
The tarball contains the minimum set of libraries needed to get
repo-install working. The bootstrap libraries (like all repo-install
libraries) are actually software repositories. Patches to these
systems (including repo-install) can be submitted according to
conventions for the given system.

Untar to a convenient location and add the following to your lisp
initialization file.

    
      (load "<i>&lt;location where you untarred&gt;</i>/repo-install/boot.lisp")
    

Repo-install needs wget installed on your system to actually move the
bits to your machine. Also, we take the philosophy that the correct
version control system to use is the one the upstream library
maintainer is using. Therefore, you will need
[Git](http://git-scm.com/), [Bazaar](http://bazaar-vcs.org/),
[Subversion](http://subversion.tigris.org/),
[Mercurial](http://mercurial.selenic.com/wiki/), and
[Darcs](http://darcs.net/) too. Using
[Macports](http://www.macports.org/) on OSX, do this:
      
      sudo port install git-core bzr subversion darcs mercurial wget

Under debian, do:

      sudo apt-get install git-core bzr subversion darcs mercurial wget

How repo-install handles different upstream repositories
--------------------------------------------------------

Repo-install adopts different strategies for maintaining local
patches, depending on the upstream repository.

The best case is if the upstream repository is a DVC. Currently
repo-install supports darcs and git. (bzr support is planned) In these
cases, we maintain our local changes in the local repository and use
the *pull* command to grab upstream patches. Patches can be submitted
to the upstream repository using appropriate technique ("darcs send"
or "git format-patch").

Upstream tarballs are downloaded and stored in a bzr repository named
"upstream" in the library's repo-installer directory. An "update-repo"
operation on these repositories will check for a new tarball, and if
one exists, update this repository.  Local patches are stored in a
repository named "local" in the same directory. When the "upstream"
repository is updated, repo-install will pull the changes to the
"local", integrating your patches. Patches can be submitted to the
upstream provider by diffing the "upstream" and "local" working
directories.
    
Finally, non-distributed version control systems simply check
out the upstream library. Local patches are maintained in your
local working directory. Patches can be submitted to the upstream
provider by getting write access to the repository, or by using
the version control system's "diff" command.
    
The Manifest
------------

Repo-install gets library download locations from a lisp file
that we call the manifest. Currently, there is only one manifest
that ships with repo-install, most-recent-manifest.lisp. This
manifest can be used to download the most recent publicly
available library versions.

A manifest is simply a set of make-instance expressions that
create CLOS objects. We currently have CLOS objects that represent
the following upstream repositories:

<table rules="all" border="1">
<tr><th>CLOS Class</th><th>Description</th></tr>
<tr><td>tarball-backed-bzr-repo</td><td>Upstream is a set of tarballs. Local patches are maintained in a bzr repository.</td></tr>
<tr><td>cliki-repo</td><td>A tarball-backed-bzr-repo who's url is grabbed from cliki.net.</td></tr>
<tr><td>darcs-repo</td><td>Upstream is a Darcs repository.</td></tr>
<tr><td>git-repo</td><td>Upstream is a Git repository.</td></tr>
<tr><td>svn-repo</td><td>Upstream is a SVN repository.</td></tr>
<tr><td>cvs-repo</td><td>Upstream is a CVS repository.</td></tr>
<tr><td>mercurial-repo</td><td>Upstream is a Mercurial repository.</td></tr>
</table>

You can look in most-recent-manifest.lisp for examples of each of these classes.

Feel free to add any libraries that you may need to this file and submit a patch.
    
Exported Functions
------------------

* `(ri:install <library>)` __DEPRECIATED__
    Simply use asdf:load-system or asdf:load-op. Repo-install
    provides a hook to these functions to download and install
    any needed dependencies.
  
    Install the given library, possibly downloading it and any
    dependencies found in its asdf system definition. If a
    particular dependency is not found in the manifest signal an
    error. In this case, you'll have to add the library to the
    manifest and try again.

* `(ri:find-repo <library>)` Find the repository for the given
library.

* `(ri:update-repo <repoi>)` Download any changes from the upstream
         repository. For distributed version control systems, this is
         equivalent to a pull.  Non-distributed version control
         systems perform an update.

* `(ri:update-all-repos)` Update all repos that have ever been downloaded on this machine.

* `(ri:repos-status <repo>)` Return a string describing any changes
	  made to the local working directory that are not yet checked
	  in. If repo-install can *reliably* detect that no changes
	  have been made, return nil.

* `(ri:all-repo-status)` Run repo-status on all locally installed
	  libraries. Print out descriptions of those that have
	  uncommitted changes.

* `(ri:local-repo-changes <repo>)` Return a string describing any
	  changes made to the local repository that are not yet
	  checked in to the upstream repository. If repo-install
	  can <em>reliably</em> detect that no changes have been made,
	  return nil.

* `(ri:all-local-repo-changes)` Run local-repo-changes on all locally
	  installed libraries. Print out descriptions of those that
	  have uncommitted changes.

Patches and Issues
------------------

Please use the [repo-install github
page](http://github.com/jpalmucci/repo-install) to submit issues and
patches.

To Do
-----

* Automatically run regression tests on update (for libraries that have regression tests).
* implement 'darcs trackdown' across all repository types.
* Code to automatically submit patches.
* Able to specify a tag or a branch on the upstream repository
* Make it easy to publish your local patches to others.
