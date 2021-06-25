# Nix configuration for `literatex-animation`
#
# Usage:
#
# * Build the program with the default compiler:
#
#     $ nix-build

{ # This string argument specifies the compiler (example: "ghc8104").  When
  # not specified, the default compiler (configured below) is used.
  compiler ? null
  # This path argument specifies the packages to use.  When not specified and
  # the default compiler is selected, a known good revision is used.
  # Otherwise, the packages configured on the filesystem are used.
, nixpkgs ? null
  # This boolean argument is used by `shell.nix`.  When `True`, build tools
  # are added to the derivation.
, isShell ? false
}:

let

  # This string defines the default compiler version.
  defaultCompiler = "ghc8104";

  # This string defines a known good revision for the default compiler.
  defaultRevision = "4d4fdc329285e0d0c1c1a2b65947d651b8ba6b29";

  # This function fetches the specified nixpkgs revision.
  nixpkgsTarball = rev:
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    };

  # The compiler is explicitly specified or the default.
  compiler' = if isNull compiler then defaultCompiler else compiler;

  # Packages are explicitly specified, those for a known good revision for the
  # default compiler, or those configured on the filesystem.
  pkgs = if isNull nixpkgs
    then if isNull compiler
      then import (nixpkgsTarball defaultRevision) {}
      else import <nixpkgs> {}
    else nixpkgs;

  # This does not work with old revisions.
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;

in

  # Configure the development environment for the package using the selected
  # packages and compiler.
  pkgs.haskell.packages.${compiler'}.developPackage {
    root = gitIgnore [./.gitignore] ./.;
    name = "literatex-animation";
    modifier = drv:
      if isShell
        then pkgs.haskell.lib.addBuildTools drv
          [ pkgs.cabal-install pkgs.gifsicle pkgs.noto-fonts
          ]
        else drv;
  }
