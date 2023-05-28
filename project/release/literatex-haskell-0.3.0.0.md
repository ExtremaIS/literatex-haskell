# `literatex-haskell` `0.3.0.0` Release Notes

Date
: 2023-05-28

## Overview

LiterateX transforms literate source code to Markdown.  Write documentation in
Markdown format in the comments of source code, and LiterateX can transform
the file to Markdown, optionally including the source code with syntax
highlighting and line numbers.

See the [README][] for details.

[README]: <https://github.com/ExtremaIS/literatex-haskell#readme>

## This Release

This release adds compatibility with the latest version of the
`optparse-applicative` library.

There are no changes to the API or CLI.

### Compatibility

To use this release with a Stackage snapshot that does not include it, add
the following to your `stack.yaml` configuration:

```yaml
extra-deps:
  - literatex-0.3.0.0
```

### Issues

There are no known issues at this time.
