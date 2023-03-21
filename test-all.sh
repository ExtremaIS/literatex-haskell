#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
#set -o xtrace

if [ "$#" -ne "1" ] ; then
  echo "Usage: test-all.sh COMMAND" 1>&2
  echo 1>&2
  echo "Commands:" 1>&2
  echo "  cabal   test all GHC versions using Cabal" 1>&2
  echo "  github  output GitHub actions matrix configuration" 1>&2
  echo "  stack   test all GHC versions using Stack" 1>&2
  exit 2
fi

ghcvers() {
  awk '/^tested-with:$/{p=1;next}/^$/{p=0}p' ./*.cabal | sed 's/^[^=]*==//'
}

ghcvers_lower() {
  grep ^with-compiler: cabal-bounds-lower.project | sed 's/.*-//'
}

ghcvers_upper() {
  grep ^with-compiler: cabal-bounds-upper.project | sed 's/.*-//'
}

mockhr="$(command -v hr >/dev/null 2>&1 && echo "0" || echo "1")"
hr() {
  echo "--{ $* }--------------------------------------"
}
test "${mockhr}" -eq "1" || unset -f hr

fail() {
  echo
  echo "Fail!"
  exit 1
}

cabaltest() {
  local ghcver="$1"
  local projfile="cabal-${ghcver}.project"
  hr "${ghcver}"
  declare -a args
  args+=("--with-ghc=ghc-${ghcver}")
  if [ -e "${projfile}" ] ; then
    args+=("--project-file=${projfile}")
  fi
  args+=("--enable-tests")
  args+=("--enable-benchmarks")
  cabal v2-build "${args[@]}" || fail
  cabal v2-test "${args[@]}" --test-show-details=always || fail
  cabal v2-haddock "${args[@]}" || fail
  cabal v2-build "${args[@]}" exe:literatex -f examples || fail
}

stacktest() {
  local ghcver="$1"
  local config="stack-${ghcver}.yaml"
  hr "${ghcver}"
  declare -a args
  args+=("--stack-yaml=${config}")
  stack build "${args[@]}" --test --bench --no-run-tests --no-run-benchmarks \
    || fail
  stack test "${args[@]}" || fail
  stack haddock "${args[@]}" || fail
  stack build "${args[@]}" --flag literatex:examples || fail
}

case "$1" in
  "cabal" )
    while IFS=$'\n' read -r ghcver ; do
      cabaltest "${ghcver}"
    done < <(ghcvers)
    ;;
  "github" )
    echo "ghcvers=$(ghcvers | jq -Rnc '[inputs]')"
    echo "ghcvers_lower=$(ghcvers_lower | jq -Rnc '[inputs]')"
    echo "ghcvers_upper=$(ghcvers_upper | jq -Rnc '[inputs]')"
    exit 0
    ;;
  "stack" )
    while IFS=$'\n' read -r ghcver ; do
      stacktest "${ghcver}"
    done < <(ghcvers)
    ;;
  * )
    echo "error: unknown command: $1" 1>&2
    exit 2
    ;;
esac

echo
echo "Success!"
