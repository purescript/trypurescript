#! /usr/bin/env bash

set -ex

# This script can be run to deploy a new version of Try PureScript.

trypurescript_version="$1"

if [ "$trypurescript_version" = "" ]
then
  echo >&2 "Need to provide a version"
  exit 1
fi

deploy_script="deploy-trypurescript.sh"
scp deploy/remote.sh "root@try.purescript.org:${deploy_script}"
ssh root@try.purescript.org "bash ${deploy_script} ${trypurescript_version}"
