# Nix shell configuration for LiterateX development
#
# Usage:
#
# * Run a Nix shell with the default compiler:
#
#     $ nix-shell
#
# * Run a Nix shell with a specific compiler version:
#
#     $ nix-shell --argstr compiler ghc901

{ # This string argument specifies the compiler (example: "ghc8107").  When
  # not specified, the default compiler is used.
  compiler ? null
  # This path argument specifies the packages to use.  When not specified, a
  # working revision for the selected compiler is used.  When a working
  # revision for the selected compiler is not defined (below), the packages
  # configured on the filesystem are used.
, nixpkgs ? null
}@args:

import ./default.nix (args // { isShell = true; })
