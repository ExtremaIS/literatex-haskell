{ pkgs ? import <nixpkgs> {} }:
let
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;
in
  pkgs.haskellPackages.developPackage {
    root = gitIgnore [./.gitignore] ./.;
    name = "literatex-animation";
  }
