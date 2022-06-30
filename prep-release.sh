#!/usr/bin/env bash

pushd staging || exit 1

spago upgrade-set

cat > spago.dhall << EOF
{ name = "try-purescript-server"
, dependencies = [] : List Text
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

EOF
spago ls packages | cut -f 1 -d ' ' | tr '\n' ' ' | xargs spago install

popd || exit 1

pushd client || exit 1

npm run updateConfigVersions

popd || exit 1
