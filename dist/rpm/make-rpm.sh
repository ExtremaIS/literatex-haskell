#!/usr/bin/env bash

set -o errexit
set -o nounset
#set -o xtrace

##############################################################################
# Library

die () {
  echo "error: ${*}" >&2
  exit 1
}

section () {
  echo "### ${*} ###"
}

##############################################################################
# Parse Arguments

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
# Confirm Environment

test -n "${RPMFULLNAME}" || die "RPMFULLNAME not set"
test -n "${RPMEMAIL}" || die "RPMEMAIL not set"

arch="$(arch)"

[ -d "/host" ] || die "/host not mounted"
[ -f "/host/${litx_source}" ] || die "source not found: ${litx_source}"

##############################################################################
# Main

section "Building .rpm"
rpmdev-setuptree
cp "/host/${litx_source}" "/home/docker/rpmbuild/SOURCES"
cd "/tmp"
tar -Jxf "/host/${litx_source}" "${litx_dir}/dist/rpm/literatex-haskell.spec"
sed \
  -e "s/{{ARCH}}/${arch}/" \
  -e "s/{{VERSION}}/${litx_version}/g" \
  -e "s/{{DATE}}/$(env LC_ALL=C date '+%a %b %d %Y')/" \
  -e "s/{{RPMFULLNAME}}/${RPMFULLNAME}/" \
  -e "s/{{RPMEMAIL}}/${RPMEMAIL}/" \
  "${litx_dir}/dist/rpm/literatex-haskell.spec" \
  > "literatex-haskell.spec"
rpmbuild -bs "literatex-haskell.spec"
rpmbuild --rebuild "/home/docker/rpmbuild/SRPMS/"*".src.rpm"
cp "/home/docker/rpmbuild/SRPMS/"*".src.rpm" "/host"
cp "/home/docker/rpmbuild/RPMS/${arch}/"*".rpm" "/host"
