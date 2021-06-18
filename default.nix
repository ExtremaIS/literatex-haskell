{ pkgs ? import <nixpkgs> {} }:
let
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;
  githubTarball = owner: repo: tag:
    builtins.fetchTarball { url = "https://github.com/${owner}/${repo}/archive/refs/tags/${repo}-${tag}.tar.gz"; };
in
  pkgs.haskellPackages.developPackage {
    root = gitIgnore [./.gitignore] ./.;
    name = "literatex";
    source-overrides = {
      ttc = githubTarball "ExtremaIS" "ttc-haskell" "1.1.0.0";
    };
  }
