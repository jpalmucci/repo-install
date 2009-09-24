#!/bin/sh

pushd ../..

sudo tar -c --exclude '*~' --exclude '*.fasl' --exclude '*.dx64fsl' -zvf /Library/WebServer/Documents/repo-install/repo-install-bootstrap.tgz \
    repo-install/asdf \
    repo-install/repo-install  \
    repo-install/trivial-shell \
    repo-install/cl-ppcre  \
    repo-install/cl-fad 

popd

sudo cp readme.html /Library/WebServer/Documents/repo-install/index.html
sudo cp style.css /Library/WebServer/Documents/repo-install/

