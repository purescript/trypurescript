#! /usr/bin/env bash

set -ex

case $COMPONENT in
  server)
    mkdir bundle
    cp $(stack path --dist-dir)/build/trypurescript/trypurescript bundle/
    cp LICENSE bundle/
    cp -r deploy/ bundle/
    cp -r staging/ bundle/
    tar czf trypurescript-server.tar.gz -C bundle/ .
    ;;
  client)
    mkdir bundle
    cp LICENSE bundle/
    cp -r client/public/ bundle/
    tar czf trypurescript-client.tar.gz -C bundle/ .
    ;;
  *)
    echo >&2 "Unrecognised component: $COMPONENT"
    exit 1
    ;;
esac
