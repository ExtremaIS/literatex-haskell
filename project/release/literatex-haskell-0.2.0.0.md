# `literatex-haskell` `0.2.0.0` Release Notes

Date
: 2021-06-25

## Overview

This release of LiterateX fixes a bug and makes changes to the [Nix][]
configuration.  There are no changes to the LiterateX API.

[Nix]: <https://nixos.org/>

### Big Fix

This release includes a fix for a bug that broke `--help` output.  The issue
only affected builds using `optparse-applicative` `0.16`, so none of the
published builds were affected.

### Nix Configuration

The Nix configuration now supports testing against the following GHC versions
using known working `nixpkgs` revisions:

* GHC 8.2.2
* GHC 8.4.4
* GHC 8.6.5
* GHC 8.8.4
* GHC 8.10.4
* GHC 9.0.1
