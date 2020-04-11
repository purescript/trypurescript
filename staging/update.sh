#! /usr/bin/env bash

# Given a package-set ID such as "psc-0.13.6-20200402", this script generates a
# `packages.dhall` and a `spago.dhall` file so that every package in the given
# set is listed in the 'dependencies' key of spago.dhall. This means that users
# of the compile server can use any package from the package set.
#
# To update to a new package set, simply rerun the script and check in the
# updated `spago.dhall` and `packages.dhall`.

package_set="$1"

if [ "$package_set" = "" ]; then
  echo >&2 "error: expected a package set name"
  exit 1
fi

echo "Creating a project which depends on the entire package set $package_set..."

cat >packages.dhall <<EOF
let upstream =
      https://github.com/purescript/package-sets/releases/download/$package_set/packages.dhall

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
EOF

cat >spago.dhall <<EOF
{ name = "try-purescript-server"
, dependencies = [ "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
EOF

# Install every package in the set
spago install $(spago ls packages | cut -f 1 -d ' ' | tr '\n' ' ')
