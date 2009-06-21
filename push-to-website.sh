#!/bin/sh

pushd ../..

sudo tar -c --exclude '*~' --exclude '*.fasl' -zvf /Library/WebServer/Documents/repo-install/repo-install-bootstrap.tgz repo-install/repo-install repo-install/asdf repo-install/split-sequence repo-install/trivial-http repo-install/usocket repo-install/systems/repo-install.asd repo-install/systems/split-sequence.asd repo-install/systems/trivial-http.asd repo-install/systems/usocket.asd

popd

sudo cp readme.html /Library/WebServer/Documents/repo-install/index.html
sudo cp style.css /Library/WebServer/Documents/repo-install/

