#!/bin/sh

pushd ../..

sudo tar -c --exclude '*~' --exclude '*.fasl' --exclude '*.dx64fsl' -zvf /Library/WebServer/Documents/repo-install/repo-install-bootstrap.tgz \
    repo-install/asdf \
    repo-install/repo-install repo-install/systems/repo-install.asd \
    repo-install/cl-ppcre repo-install/systems/cl-ppcre.asd \
    repo-install/cl-fad repo-install/systems/cl-fad.asd \
    repo-install/trivial-http repo-install/systems/trivial-http.asd \
    repo-install/usocket repo-install/systems/usocket.asd

popd

sudo cp readme.html /Library/WebServer/Documents/repo-install/index.html
sudo cp style.css /Library/WebServer/Documents/repo-install/

