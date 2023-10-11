# `literatex-haskell` TODO

## Functionality

* Add special support for handling pragmas?

## Tests

## Compatibility

* GHC 9.8.1
    * Building with Cabal works without issue using `allower-newer`
      configuration like in `cabal-bounds-upper.project`.
    * Building with Stack fails when attempting to build examples.  I will
      test against GHC 9.8.1 once `allow-newer` configuration is no longer
      required.
* Remove `allow-newer` entries in `cabal-bounds-upper.project`
    * [`exceptions`](https://hackage.haskell.org/package/exceptions)
      when support `template-haskell` `2.21.0.0`
    * [`safe-exceptions`](https://hackage.haskell.org/package/safe-exceptions)
      when support `deepseq` `1.5.0.0`

## Documentation

## Examples

## Project

* Test examples with Stack in CI (currently disabled because setting the
  `examples` flag on the command line disables the
  `optparse-applicative_ge_0_18` flag when set in the `stack.yaml`
  configuration)
* Rewrite animation with [reanimate](https://github.com/reanimate/reanimate)?
