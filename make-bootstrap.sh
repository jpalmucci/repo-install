#!/bin/sh

pushd ../..

tar czvf ~/repo-install-bootstrap.tgz repo-install/repo-install repo-install/split-sequence repo-install/trivial-http repo-install/usocket repo-install/systems/repo-install.asd repo-install/systems/split-sequence.asd repo-install/systems/trivial-http.asd repo-install/systems/usocket.asd

popd