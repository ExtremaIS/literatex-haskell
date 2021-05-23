LiterateX High-Level Example
============================

This is a simple example of using the [LiterateX][] high-level API to
transform Haskell source code to [Pandoc Markdown][].

[LiterateX]: <https://github.com/ExtremaIS/literatex-haskell#readme>
[Pandoc Markdown]: <https://pandoc.org/MANUAL.html#pandocs-markdown>

Code
----

When executed, this example program reads this source code and transforms it
to Markdown, writing the output to `STDOUT`.  Note that the path is
hard-coded, so be sure to execute the program from the project directory.

The `OverloadedStrings` extension is used to set the code language below.

> {-# LANGUAGE OverloadedStrings #-}
>
> module Main (main) where

Imports from [base](https://hackage.haskell.org/package/base):

> import System.IO (stdout)

Imports from [literatex](https://hackage.haskell.org/package/literatex):

> import qualified LiterateX
> import qualified LiterateX.Renderer as Renderer
> import qualified LiterateX.Types.SourceFormat as SourceFormat

The `main` function is just a call to `LiterateX.transformFileToHandle` with
appropriate options.

> main :: IO ()
> main = LiterateX.transformFileToHandle
>     SourceFormat.LiterateHaskell
>     (Renderer.defaultOptionsFor "haskell")
>     "examples/highlevel.lhs"
>     stdout
