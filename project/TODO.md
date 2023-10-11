# `literatex-haskell` TODO

## Functionality

* Add special support for handling pragmas?

## Tests

## Compatibility

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
