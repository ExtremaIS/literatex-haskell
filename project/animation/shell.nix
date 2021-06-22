# Nix shell configuration for `literatex-animation` development
#
# Usage:
#
# * Run a Nix shell with the default compiler:
#
#     $ nix-shell

{ # This string argument specifies the compiler (example: "ghc8104").  When
  # not specified, the default compiler is used.
  compiler ? null
  # This path argument specifies the packages to use.  When not specified and
  # the default compiler is selected, a known good revision is used.
  # Otherwise, the packages configured on the filesystem are used.
, nixpkgs ? null
}@args:

import ./default.nix (args // { isShell = true; })
