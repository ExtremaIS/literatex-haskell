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

{ # This string argument specifies the compiler (example: "ghc8104").  When
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
  defaultCompiler = "ghc8104";

  # This set defines working revisions for supported compiler versions.
  nixpkgsRevs = {
    ghc901  = "4d4fdc329285e0d0c1c1a2b65947d651b8ba6b29";
    ghc8104 = "c92ca95afb5043bc6faa0d526460584eccff2277";
    ghc884  = "c92ca95afb5043bc6faa0d526460584eccff2277";
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

  # Git ignore functionality must use the most recent revision.
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
      ttc = githubTagTarball "ExtremaIS" "ttc-haskell" "ttc-haskell-1.1.0.0";
    };
    modifier = drv:
      if isShell
        then pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
          [ cabal-install
          ])
        else drv;
  }
