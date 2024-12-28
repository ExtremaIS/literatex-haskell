# `literatex-haskell` TODO

## Functionality

* Add special support for handling pragmas?

## Tests

## Compatibility

* GHC 9.12.1 blocked by [`ttc`][]
    * TTC now builds using GHC 9.12.1 without issue using Cabal.  Stack 3.1.1
      has issues, but they should be resolved in Stack 3.3.1 (already
      released).  When Stack 3.3.1 is added to GHCup, I will confirm, push,
      and make a Hackage revision.

[`ttc`]: <https://hackage.haskell.org/package/ttc>

## Documentation

## Examples

## Project

* Test examples with Stack in CI (currently disabled because setting the
  `examples` flag on the command line disables the
  `optparse-applicative_ge_0_18` flag when set in the `stack.yaml`
  configuration)
* Rewrite animation with [reanimate](https://github.com/reanimate/reanimate)?
