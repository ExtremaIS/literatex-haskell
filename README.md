# LiterateX

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GitHub CI](https://github.com/ExtremaIS/literatex-haskell/workflows/CI/badge.svg?branch=main)](https://github.com/ExtremaIS/literatex-haskell/actions)
[![Hackage](https://img.shields.io/hackage/v/literatex.svg)](https://hackage.haskell.org/package/literatex)
[![Stackage LTS](https://stackage.org/package/literatex/badge/lts)](https://stackage.org/package/literatex)
[![Stackage Nightly](https://stackage.org/package/literatex/badge/nightly)](https://stackage.org/nightly/package/literatex)

![LiterateX: LiterateBash, LiterateC, LiterateClojure, LiterateErlang, LiterateHaskell, LiterateIdris, LiterateJavaScript, LiteratePython, LiterateRacket, LiterateSQL, Literate...](project/animation/literatex.gif)

* [Overview](#overview)
* [Source Formats](#source-formats)
    * [Double-Dash Comments](#double-dash-comments)
    * [Double-Slash Comments](#double-slash-comments)
    * [Hash Comments](#hash-comments)
    * [Lisp Semicolon Comments](#lisp-semicolon-comments)
    * [Literate Haskell](#literate-haskell)
    * [Percent Comments](#percent-comments)
* [CLI](#cli)
    * [Requirements](#requirements)
    * [Installation](#installation)
        * [`.deb` Package Installation](#deb-package-installation)
        * [`.rpm` Package Installation](#rpm-package-installation)
        * [Installation From Hackage](#installation-from-hackage)
        * [Installation From Stackage](#installation-from-stackage)
    * [Usage](#usage)
* [Library](#library)
* [Related Work](#related-work)
* [Project](#project)
    * [Links](#links)
    * [Tags](#tags)
    * [Contribution](#contribution)
    * [License](#license)

## Overview

LiterateX transforms literate source code to Markdown.  Write documentation in
Markdown format in the comments of source code, and LiterateX can transform
the file to Markdown, optionally including the source code with syntax
highlighting and line numbers.  Many [source formats](#source-formats) are
supported, and the following target formats are supported:

* [Pandoc Markdown][]
* [GitHub Flavored Markdown][]
* [mdBook Markdown][], which does not support per-block line numbering

LiterateX can be used to document code that is particularly important or
difficult to understand.  For example, documentation can be written in the SQL
file that defines the schema for a complex database.  LiterateX can translate
the `.sql` file to a Markdown file, which can then be converted to HTML, PDF,
or even EPUB with [Pandoc][].

LiterateX can also be used in the publishing of blog entries, magazine
articles, or books.  Use the [command-line utility](#cli) or integrate the
Haskell [library](#library) into your own software.

LiterateX has support for **source code rules**, comment lines that are used
to visually separate sections of source code.  Since source code rules are
only used to make the source code easier to scan quickly, they are ignored
(treated as a blank line) when translating to Markdown.

LiterateX also has support for [shebang][] lines at the start of the file.
They can be ignored so that they do not precede the documentation.

[Pandoc Markdown]: <https://pandoc.org/MANUAL.html#pandocs-markdown>
[GitHub Flavored Markdown]: <https://github.github.com/gfm/>
[mdBook Markdown]: <https://rust-lang.github.io/mdBook/>
[Pandoc]: <https://pandoc.org/>
[shebang]: <https://en.wikipedia.org/wiki/Shebang_(Unix)>

## Source Formats

LiterateX supports a number of source formats.  With the exception of
[literate Haskell](#literate-haskell), documentation is written in line
comments in the source language.  Note that multi-line comments are treated as
code, not documentation.

### Double-Dash Comments

Documentation is parsed from lines that begin with two dashes immediately
followed by a space (`-- `).  Lines that only contain two dashes are treated
as blank lines in the documentation.  Lines that contain three or more dashes
are treated as source code rules and are ignored.

``` haskell
-- # Haskell Example
--
-- Executables are implemented using a `Main` module that exposes a function
-- named `main`.

module Main (main) where

-- The `main` function is run when the program is executed.

main :: IO ()
main = putStrLn "Hello!"

-- This simple example just prints "Hello!" to the screen.
```

Languages that use double-dash comments include the following:

* [Elm][]
* [Haskell][]
* [Idris][]
* [Lua][]
* [SQL][]

[Elm]: <https://elm-lang.org/>
[Haskell]: <https://www.haskell.org/>
[Idris]: <https://www.idris-lang.org/>
[Lua]: <http://www.lua.org/>
[SQL]: <https://en.wikipedia.org/wiki/SQL>

### Double-Slash Comments

Documentation is parsed from lines that begin with two slashes immediately
followed by a space (`// `).  Lines that only contain two slashes are treated
as blank lines in the documentation.  Lines that contain three or more slashes
are treated as source code rules and are ignored.

``` rust
// # Rust Example
//
// The `main` function is run when the program is executed.

fn main() {
    println!("Hello!");
}

// This simple example just prints "Hello!" to the screen.
```

Languages that use double-slash comments include the following:

* [C][]
* [CSS][]
* [Go][]
* [Java][]
* [JavaScript][]
* [Kotlin][]
* [PHP][]
* [Rust][]
* [Scala][]
* [TypeScript][]

[C]: <https://en.wikipedia.org/wiki/C_(programming_language)>
[CSS]: <https://en.wikipedia.org/wiki/CSS>
[Go]: <https://golang.org/>
[Java]: <https://www.java.com/>
[JavaScript]: <https://en.wikipedia.org/wiki/JavaScript>
[Kotlin]: <https://kotlinlang.org/>
[PHP]: <https://www.php.net/>
[Rust]: <https://www.rust-lang.org/>
[Scala]: <https://www.scala-lang.org/>
[TypeScript]: <https://www.typescriptlang.org/>

### Hash Comments

Documentation is parsed from lines that begin with a hash character
immediately followed by a space (`# `).  Lines that only contain a hash
character are treated as blank lines in the documentation.  Lines that contain
two or more hash characters are treated as source code rules and are ignored.

``` python
# # Python Example
#
# A quick-and-dirty Python script can include commands at the top level!

print("Hello!")

# This simple example just prints "Hello!" to the screen.
```

Languages that use hash comments include the following:

* [Bash][]
* [Elixir][]
* [Perl][]
* [Python][]
* [R][]
* [Ruby][]

[Bash]: <https://www.gnu.org/software/bash/>
[Elixir]: <https://elixir-lang.org/>
[Perl]: <https://www.perl.org/>
[Python]: <https://www.python.org/>
[R]: <https://www.r-project.org/>
[Ruby]: <https://www.ruby-lang.org/>

### Lisp Semicolon Comments

Lisp languages use a semicolon (`;`) for line comments, but there is a
special convention to use a different number of semicolons according to the
context.  Documentation is parsed from lines that begin with one to four
semicolons immediately followed by a space (`; `, `;; `, `;;; `, or `;;;; `).
Lines that only contain one to four semicolons are treated as blank lines in
the documentation.  Lines that contain more than four semicolons are treated
as source code rules and are ignored.

``` scheme
; # Racket Example
;
; Racket programs must declare the language to use.

#lang racket/base

; Racket can also include commands at the top level!

(println "Hello!")

; This simple example just prints "Hello!" to the screen.
```

Languages that use Lisp semicolon comments include the following:

* [Clojure][]
* [Common Lisp][]
* [Racket][]
* [Scheme][]

[Clojure]: <https://clojure.org/>
[Common Lisp]: <https://common-lisp.net/>
[Racket]: <https://racket-lang.org/>
[Scheme]: <https://en.wikipedia.org/wiki/Scheme_(programming_language)>

### Literate Haskell

[GHC][] has special support for [literate programming][].  Haskell source code
is usually written with documentation in comments, in files with a `.hs`
extension.  Literate Haskell source code gives documentation the leading role
and prefixes code with a greater-than sign and space (`> `), in files with a
`.lhs` extension.  The documentation can be in any format, and [Markdown][] is
a popular choice.

Unfortunately, there is a [bug][] that causes problems when using
[ATX-style headings][] (`#` characters before the heading text).  Any easy
workaround is to use [setext-style headings][] (underlines) instead, but this
limits the number of heading levels.  See the
[Literate Haskell Markdown Headings][] blog post for more information and an
example workaround.

This source format does not support source code rules.

``` lhaskell
Literate Haskell Example
========================

Executables are implemented using a `Main` module that exposes a function
named `main`.

> module Main (main) where

The `main` function is run when the program is executed.

> main :: IO ()
> main = putStrLn "Hello!"

This simple example just prints "Hello!" to the screen.
```

[GHC]: <https://www.haskell.org/ghc/>
[literate programming]: <https://wiki.haskell.org/Literate_programming>
[Markdown]: <https://en.wikipedia.org/wiki/Markdown>
[bug]: <https://gitlab.haskell.org/ghc/ghc/-/issues/4836>
[ATX-style headings]: <https://pandoc.org/MANUAL.html#atx-style-headings>
[setext-style headings]: <https://pandoc.org/MANUAL.html#setext-style-headings>
[Literate Haskell Markdown Headings]: <https://www.extrema.is/blog/2023/03/21/literate-haskell-markdown-headings>

### Percent Comments

Documentation is parsed from lines that begin with a percent character
immediately followed by a space (`% `).  Lines that only contain a percent
character are treated as blank lines in the documentation.  Lines that contain
two or more percent characters are treated as source code rules and are
ignored.

``` erlang
% # Erlang Example
%
% Programs are implemented using a `main` module that exports a `start/0`
% function.

-module(main).
-export([start/0]).

% The `start` function is run when the program is executed.

start() -> io.fwrite("Hello!\n").

% This simple example just prints "Hello!" to the screen.
```

Languages that use percent comments include the following:

* [Erlang][]
* [LaTeX][]

[Erlang]: <https://www.erlang.org/>
[LaTeX]: <https://www.latex-project.org/>

## CLI

LiterateX may be used via a command-line utility named `literatex`.

### Requirements

`literatex` has only been tested on Linux.  It *might* work on Windows and
macOS.

### Installation

#### `.deb` Package Installation

Check the [Releases][] page for `.deb` packages.

[Releases]: <https://github.com/ExtremaIS/literatex-haskell/releases>

#### `.rpm` Package Installation

Check the [Releases][] page for `.rpm` packages.

#### Installation From Hackage

Install `literatex` from [Hackage][] using [Cabal][] as follows:

```
$ cabal v2-install literatex
```

[Hackage]: <https://hackage.haskell.org/package/literatex>
[Cabal]: <https://www.haskell.org/cabal/>

#### Installation From Stackage

Install `literatex` from [Stackage][] using [Stack][] as follows:

```
$ stack install literatex
```

[Stackage]: <https://www.stackage.org/package/literatex>
[Stack]: <https://haskellstack.org/>

### Usage

See the [`literatex` man page][] for usage information.

[`literatex` man page]: <doc/literatex.1.md>

## Library

The [LiterateX Haskell library][] provides an API for integrating LiterateX
functionality in your own software.

[LiterateX Haskell library]: <https://hackage.haskell.org/package/literatex>

## Related Work

[Literate programming][] is a style of programming introduced by
[Donald Knuth][] in which [the main idea][] is "to regard a program as a
communication to human beings rather than as a set of instructions to a
computer."  LiterateX is faithful to this idea in that it is used for
communication to human beings.  Note, however, that LiterateX does *not*
support another core aspect of Knuth's literate programming: the ability to
write source code in the order best for human understanding.  Since LiterateX
transforms actual source code files, the source code has to be written in
whatever order is required by the language.  Those interested in writing code
in different order are encouraged to check out [noweb][] and [CWEB][].

The [lhs2tex][] utility is used to work with literate Haskell and LaTeX.

The [markdown-unlit][] utility is used to extract Haskell code from Markdown
files.  This is useful in cases where the Markdown file is displayed on
GitHub.

The [src2md][] utility, written in Common Lisp, also supports multiple source
formats.  It outputs Markdown that includes HTML, which limits the usefulness
of the Markdown.

The [extract-documentation-comments][] utility, written in JavaScript,
extracts documentation from multi-line JavaScript comments.

[mlp.clj][], written in Clojure, is a [babashka][] script that transforms
literate Clojure source code to Markdown, including HTML.  The author uses it
to implement a [live preview][] of literate Clojure documentation while using
the [Notepad++][] (Windows editor).

[Literate programming]: <https://en.wikipedia.org/wiki/Literate_programming>
[Donald Knuth]: <https://en.wikipedia.org/wiki/Donald_Knuth>
[the main idea]: <https://www-cs-faculty.stanford.edu/~knuth/cweb.html>
[noweb]: <https://en.wikipedia.org/wiki/Noweb>
[CWEB]: <https://en.wikipedia.org/wiki/CWEB>
[lhs2tex]: <https://github.com/kosmikus/lhs2tex>
[markdown-unlit]: <https://github.com/sol/markdown-unlit>
[src2md]: <https://git.sr.ht/~aerique/src2markup>
[extract-documentation-comments]: <https://github.com/Anadian/extract-documentation-comments#readme>
[mlp.clj]: <https://github.com/linpengcheng/ClojureBoxNpp/blob/master/Notepad%2B%2B/tools/clj/mlp.clj>
[babashka]: <https://github.com/babashka/babashka#readme>
[live preview]: <https://github.com/linpengcheng/ClojureBoxNpp/tree/master/Notepad%2B%2B/plugins/Config/PreviewHTML>
[Notepad++]: <https://notepad-plus-plus.org/>

## Project

### Links

* GitHub: <https://github.com/ExtremaIS/literatex-haskell>

### Branches

The `main` branch is reserved for releases.  It may be considered stable, and
`HEAD` is always the latest release.

The `develop` branch is the primary development branch.  It contains changes
that have not yet been released, and it is not necessarily stable.

[Hackage revisions][] are made for metadata changes, such as relaxation of
constraints when new versions of dependencies are released.  The
`literatex.cabal` metadata in the `main` branch may therefore not match that
of Hackage.  The `literatex.cabal` metadata in the `develop` branch may match,
*unless* work is being done on a new release that contains other changes.

[Hackage revisions]: <https://github.com/haskell-infra/hackage-trustees/blob/master/revisions-information.md#hackage-metadata-revisions--what-they-are-how-they-work>

### Tags

All releases are tagged in the `main` branch.  Release tags are signed using
the [`security@extrema.is` GPG key][].

[`security@extrema.is` GPG key]: <https://keyserver.ubuntu.com/pks/lookup?search=0x1D484E4B4705FADF&fingerprint=on&op=index>

### Contribution

Issues and feature requests are tracked on GitHub:
<https://github.com/ExtremaIS/literatex-haskell/issues>

Issues may also be submitted via email to <bugs@extrema.is>.

### License

This project is released under the [MIT License][] as specified in the
[`LICENSE`][] file.

[MIT License]: <https://opensource.org/licenses/MIT>
[`LICENSE`]: <LICENSE>
