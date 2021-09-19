# Nix configuration for testing LiterateX against all supported GHC versions
#
# Usage:
#
#     $ nix-build test-all.nix

{
  literatex-ghc-822 = import ./default.nix { compiler = "ghc822"; };
  literatex-ghc-844 = import ./default.nix { compiler = "ghc844"; };
  literatex-ghc-865 = import ./default.nix { compiler = "ghc865"; };
  literatex-ghc-884 = import ./default.nix { compiler = "ghc884"; };
  literatex-ghc-8107 = import ./default.nix { compiler = "ghc8107"; };
  literatex-ghc-901 = import ./default.nix { compiler = "ghc901"; };
}
