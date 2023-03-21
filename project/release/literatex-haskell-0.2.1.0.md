# `literatex-haskell` `0.2.1.0` Release Notes

Date
: 2023-03-21

## Overview

LiterateX transforms literate source code to Markdown.  Write documentation in
Markdown format in the comments of source code, and LiterateX can transform
the file to Markdown, optionally including the source code with syntax
highlighting and line numbers.

See the [README][] for details.

[README]: <https://github.com/ExtremaIS/literatex-haskell#readme>

## This Release

This release adds support for the [mdBook Markdown][] output format.

LiterateX `0.2.0.2` was released more than one year ago, and a number of
updates to dependency constraints have since been registered as
[Hackage revisions][].  This release also updates the package (tarball and
`main` branch) to the latest state.

This release also includes changes to the project management infrastructure.
One important change is that both lower and upper bounds of dependencies are
now tested in CI.

[mdBook Markdown]: <https://rust-lang.github.io/mdBook/>
[Hackage revisions]: <https://github.com/haskell-infra/hackage-trustees/blob/master/revisions-information.md#hackage-metadata-revisions--what-they-are-how-they-work>

### mdBook Markdown support

The [mdBook Markdown][] format does not support per-block line numbering.
When using this format, line numbering is not configured even when requested.

### Compatibility

Build software:

| Software          | LiterateX 0.2.0.1 | LiterateX 0.2.1.0 |
| ----------------- | ----------------- | ----------------- |
| [GHC][]           | 8.2.2 ~ 9.2.1     | 8.2.2 ~ 9.6.1     |
| [cabal-install][] | 1.24 ~ 3.6        | 1.24 ~ 3.10       |

Library dependencies:

| Package        | LiterateX 0.2.0.1   | LiterateX 0.2.1.0   |
| -------------- | ------------------- | ------------------- |
| [base][]       | `>=4.7 && <5`       | `>=4.10.1 && <4.19` |
| [bytestring][] | `>=0.10.8 && <0.12` | `>=0.10.8 && <0.12` |
| [conduit][]    | `>=1.3 && <1.4`     | `>=1.3 && <1.4`     |
| [text][]       | `>=1.2.3 && <2.1`   | `>=1.2.3 && <2.1`   |
| [ttc][]        | `>=0.4 && <1.2`     | `>=0.4 && <1.3`     |
| [unliftio][]   | `>=0.2 && <0.3`     | `>=0.2 && <0.3`     |

Executable dependencies:

| Package                  | LiterateX 0.2.0.1 | LiterateX 0.2.1.0 |
| ------------------------ | ----------------- | ----------------- |
| [ansi-wl-pprint][]       | `>=0.6 && <0.7`   | `>=0.6.8 && <0.7` |
| [optparse-applicative][] | `>=0.14 && <0.18` | `>=0.13 && <0.18` |

Test dependencies:

| Package         | LiterateX 0.2.0.1 | LiterateX 0.2.1.0   |
| --------------- | ----------------- | ------------------- |
| [filepath][]    | (unconstrained)   | `>=1.4.1.2 && <1.5` |
| [tasty][]       | (unconstrained)   | `>=0.12 && <1.5`    |
| [tasty-hunit][] | (unconstrained)   | `>=0.8 && <0.11`    |

To use this release with a Stackage snapshot that does not include it, add
the following to your `stack.yaml` configuration:

```yaml
extra-deps:
  - literatex-0.2.1.0
```

[GHC]: <https://www.haskell.org/ghc/>
[cabal-install]: <https://hackage.haskell.org/package/cabal-install>
[base]: <https://hackage.haskell.org/package/base>
[bytestring]: <https://hackage.haskell.org/package/bytestring>
[conduit]: <https://hackage.haskell.org/package/conduit>
[text]: <https://hackage.haskell.org/package/text>
[ttc]: <https://hackage.haskell.org/package/ttc>
[unliftio]: <https://hackage.haskell.org/package/unliftio>
[ansi-wl-pprint]: <https://hackage.haskell.org/package/ansi-wl-pprint>
[optparse-applicative]: <https://hackage.haskell.org/package/optparse-applicative>
[filepath]: <https://hackage.haskell.org/package/filepath>
[tasty]: <https://hackage.haskell.org/package/tasty>
[tasty-hunit]: <https://hackage.haskell.org/package/tasty-hunit>

### Issues

There are no known issues at this time.
