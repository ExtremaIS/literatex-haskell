{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  inputsFrom = [ (import ./default.nix { inherit pkgs; }) ];
  buildInputs = [ pkgs.gifsicle pkgs.cabal-install pkgs.noto-fonts ];
}
