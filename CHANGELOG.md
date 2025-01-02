# `literatex-haskell` Changelog

This project follows the [Haskell package versioning policy][PVP], with
versions in `A.B.C.D` format.  `A` may be incremented arbitrarily for
non-technical reasons, but [semantic versioning][SemVer] is otherwise
followed, where `A.B` is the major version, `C` is the minor version, and `D`
is the patch version.  Initial development uses versions `0.0.C.D`, for which
every version is considered breaking.

[PVP]: <https://pvp.haskell.org/>
[SemVer]: <https://semver.org/>

The format of this changelog is based on [Keep a Changelog][KaC], with the
following conventions:

* Level-two heading `Unreleased` is used to track changes that have not been
  released.
* Other level-two headings specify the release in `A.B.C.D (YYYY-MM-DD)`
  format, with newer versions above older versions.
* Level-three headings are used to categorize changes as follows:
    1. Breaking
    2. Non-Breaking
* Changes are listed in arbitrary order and present tense.

[KaC]: <https://keepachangelog.com/en/1.0.0/>

## 0.4.0.0 (2025-01-03)

### Breaking

* Remove support for GHC 8.6, constraining lower bounds
* Remove support for GHC 8.4, constraining lower bounds
* Remove support for GHC 8.2, constraining lower bounds
* Change minimal Cabal from 1.24 to 3.0

### Non-Breaking

* Bump `base` dependency version upper bound
* Bump `bytestring` dependency version upper bound
* Bump `filepath` dependency version upper bound
* Bump `tasty` dependency version upper bound
* Bump `text` dependency version upper bound
* Bump `ttc` dependency version upper bound

## 0.3.0.0 (2023-05-28)

## Breaking

* Add support for `optparse-applicative` `0.18`

### Non-Breaking

* Bump `ansi-wl-pprint` dependency version upper bound

## 0.2.1.0 (2023-03-21)

### Breaking

* Add `MdBook` target format

### Non-Breaking

* Bump TTC dependency version upper bound
* Adjust dependency constraints to match tested versions

## 0.2.0.2 (2022-02-05)

### Non-Breaking

* Bump `optparse-applicative` dependency version upper bound

## 0.2.0.1 (2021-12-25)

### Non-Breaking

* Bump `text` dependency version upper bound

## 0.2.0.0 (2021-06-25)

### Breaking

* Fix `--help` when using `optparse-applicative` `0.16`

### Non-Breaking

* Refactor Nix configuration
* Use TTC 1.1.0.1

## 0.1.0.2 (2021-06-10)

### Non-Breaking

* Bump TTC dependency version upper bound

## 0.1.0.1 (2021-06-03)

### Non-Breaking

* Use `docker-pkg` scripts to build packages
* Bump TTC dependency version upper bound

## 0.1.0.0 (2021-05-26)

### Breaking

* Initial public release
