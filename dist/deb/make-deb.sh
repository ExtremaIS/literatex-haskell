#!/usr/bin/env bash

set -o errexit
set -o nounset
#set -o xtrace

##############################################################################
# constants

term_bold="$(tput bold)"
term_reset="$(tput sgr0)"

##############################################################################
# library

die () {
  echo "error: ${*}" >&2
  exit 1
}

section () {
  echo "${term_bold}${*}${term_reset}"
}

##############################################################################
# parse arguments

if [ "$#" -ne "1" ] ; then
  echo "usage: ${0} literatex-haskell-VERSION.tar.xz" >&2
  exit 2
fi

litx_source="${1}"
litx_dir="${litx_source%.tar.xz}"
[ "${litx_dir}" != "${litx_source}" ] \
  || die "invalid source filename: ${litx_source}"
litx_version="${litx_dir#literatex-haskell-}"
[ "${litx_version}" != "${litx_dir}" ] \
  || die "invalid source filename: ${litx_source}"

##############################################################################
# confirm environment

test -n "${DEBFULLNAME}" || die "DEBFULLNAME not set"
test -n "${DEBEMAIL}" || die "DEBEMAIL not set"

arch="$(dpkg --print-architecture)"

[ -d "/host" ] || die "/host not mounted"
[ -f "/host/${litx_source}" ] || die "source not found: ${litx_source}"

##############################################################################
# main

section "Building .deb"
cd "/tmp"
tar -Jxf "/host/${litx_source}"
[ -d "${litx_dir}" ] || die "source directory not found: ${litx_dir}"
cd "${litx_dir}"
dh_make --single --yes -f "/host/${litx_source}"
cd "debian"
rm -rf README.* literatex-haskell* ./*.ex source
sed -i "s/^  \\*.*/  * Release ${litx_version}/" changelog
cd ..
sed "s/{ARCH}/${arch}/" dist/deb/control > debian/control
cp dist/deb/copyright debian
cp dist/deb/Makefile .
dpkg-buildpackage -us -uc
cd "/tmp"
rm -rf "${litx_dir}"
sudo -u docker cp literatex-haskell* /host
