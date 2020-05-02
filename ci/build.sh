#! /usr/bin/env bash

set -ex

case $COMPONENT in
  server)
    # Set a timeout of 35 minutes. We could use travis_wait here, but travis_wait
    # doesn't produce any output until the command finishes, and also doesn't
    # always show all of the command's output.
    timeout 35m stack --no-terminal -j1 --install-ghc build
    ;;
  client)
    cd client
    npm install
    # Use production config, since we want to use these bundles for deploys
    npm_config_configpath="config/prod/*.purs" npm run build
    ;;
  *)
    echo >&2 "Unrecognised component: $COMPONENT"
    exit 1
    ;;
esac
