#! /usr/bin/env bash

set -ex

# This script should be run on the trypurescript server to deploy a new
# version. It does not attempt to take care of any of the following:
#
# - configuration of secrets/credentials
# - nginx SSL configuration
#
# so whenever any of these are needed, they must be done manually.

if [ $(id --user) -ne 0 ]
then
  echo >&2 "This script must be run as root"
  exit 1
fi

trypurescript_version="$1"

if [ "$trypurescript_version" = "" ]
then
  echo >&2 "Need to provide a version"
  exit 1
fi

download_url_base="https://github.com/purescript/trypurescript/releases/download/${trypurescript_version}"

echo "[$(date)] $0: starting trypurescript install"

# set up directories for deploying into
if [ ! -d /var/www/trypurescript ]; then
  mkdir -p /var/www/trypurescript
  chown -R www-data:www-data /var/www/trypurescript
fi

# download release
tmpdir="$(sudo -u www-data mktemp -d)"
pushd "$tmpdir"
for component in server client; do
  bundle="trypurescript-${component}.tar.gz"
  sudo -u www-data wget "$download_url_base/${bundle}"
  sudo -u www-data tar xzf "$bundle" -C /var/www/trypurescript --overwrite
done
# We install the binary to a location outside of /var/www/trypurescript so that we
# can extract tar.gz files into /var/www/trypurescript safely in future deploys.
install /var/www/trypurescript/trypurescript /usr/local/bin/trypurescript
popd
rm -r "$tmpdir"

# install nginx config
cp /var/www/trypurescript/deploy/nginx.conf /etc/nginx/sites-enabled/trypurescript.conf
systemctl reload nginx

# install systemd service confing
cp /var/www/trypurescript/deploy/trypurescript.service /etc/systemd/system/trypurescript.service
systemctl daemon-reload
systemctl restart trypurescript.service

echo "[$(date)] $0: done trypurescript install"
