# Nix configuration for LiterateX
#
# Usage:
#
# * Build LiterateX with the default compiler:
#
#     $ nix-build
#
# * Build LiterateX with a specific compiler version:
#
#     $ nix-build --argstr compiler ghc901

{ # This string argument specifies the compiler (example: "ghc8107").  When
  # not specified, the default compiler (configured below) is used.
  compiler ? null
  # This path argument specifies the packages to use.  When not specified, a
  # working revision for the selected compiler is used.  When a working
  # revision for the selected compiler is not defined (below), the packages
  # configured on the filesystem are used.
, nixpkgs ? null
  # This boolean argument is used by `shell.nix`.  When `True`, build tools
  # are added to the derivation.
, isShell ? false
}:

let

  # This string defines the default compiler version.
  defaultCompiler = "ghc8107";

  # This set defines working revisions for supported compiler versions.
  nixpkgsRevs = {
    ghc901  = "b7d0ebd8f898c9a4b55653d2fefd12319f1bc3cf";
    ghc8107 = "b7d0ebd8f898c9a4b55653d2fefd12319f1bc3cf";
    ghc884  = "b7d0ebd8f898c9a4b55653d2fefd12319f1bc3cf";
    ghc865  = "2d9888f61c80f28b09d64f5e39d0ba02e3923057";
    ghc844  = "6a80140fdf2157d1a5500a04c87033c0dcd6bf9b";
    ghc822  = "6a80140fdf2157d1a5500a04c87033c0dcd6bf9b";
  };

  # This function fetches the specified nixpkgs revision.
  nixpkgsTarball = rev:
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    };

  # This function fetches source from GitHub by tag.
  githubTagTarball = owner: repo: tag:
    builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/refs/tags/${tag}.tar.gz";
    };

  # The compiler is explicitly specified or the default.
  compiler' = if isNull compiler then defaultCompiler else compiler;

  # Packages are explicitly specified, those for the revision defined for the
  # selected compiler, or those configured on the filesystem.
  pkgs = if isNull nixpkgs
    then if nixpkgsRevs ? ${compiler'}
      then import (nixpkgsTarball nixpkgsRevs.${compiler'}) {}
      else import <nixpkgs> {}
    else nixpkgs;

  # Git ignore functionality from a fixed `nixpkgs` revision is used.  Old
  # revisions do not work, proably due to an API change.  The `ghc901` build
  # fails if that revision is not used.
  gitIgnore = (
    import (nixpkgsTarball nixpkgsRevs.ghc901) {}
  ).nix-gitignore.gitignoreSourcePure;

in

  # Configure the development environment for the package using the selected
  # packages and compiler.
  pkgs.haskell.packages.${compiler'}.developPackage {
    root = gitIgnore [./.gitignore] ./.;
    name = "literatex";
    source-overrides = {
      ttc = githubTagTarball "ExtremaIS" "ttc-haskell" "ttc-haskell-1.1.0.2";
    };
    modifier = drv:
      if isShell
        then pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
          [ cabal-install
          ])
        else drv;
  }
