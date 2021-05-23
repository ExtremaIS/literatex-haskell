# LiterateX

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![GitHub CI](https://github.com/ExtremaIS/literatex-haskell/workflows/CI/badge.svg?branch=main)](https://github.com/ExtremaIS/literatex-haskell/actions)
[![Hackage](https://img.shields.io/hackage/v/literatex.svg)](https://hackage.haskell.org/package/literatex)
[![Stackage LTS](https://stackage.org/package/literatex/badge/lts)](https://stackage.org/package/literatex)
[![Stackage Nightly](https://stackage.org/package/literatex/badge/nightly)](https://stackage.org/nightly/package/literatex)

![LiterateX](project/animation/literatex.gif)

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
        * [Installation From Source](#installation-from-source)
    * [Usage](#usage)
* [Library](#library)
* [Related Work](#related-work)
* [Project](#project)
    * [Links](#links)
    * [Releases](#releases)
    * [Contribution](#contribution)
    * [License](#license)

## Overview

LiterateX transforms literate source code to Markdown.  Write documentation in
Markdown format in the comments of source code, and LiterateX can transform
the file to Markdown, optionally including the source code with syntax
highlighting and line numbers.  Many [source formats](#source-formats) are
supported, and the following target formats are supported:

* [Pandoc Markdown](https://pandoc.org/MANUAL.html#pandocs-markdown)
* [GitHub Flavored Markdown](https://github.github.com/gfm/)

LiterateX can be used to document code that is particularly important or
difficult to understand.  For example, documentation can be written in the SQL
file that defines the schema for a complex database.  LiterateX can translate
the `.sql` file to a Markdown file, which can then be converted to HTML, PDF,
or even EPUB with [Pandoc][].

[Pandoc]: <https://pandoc.org/>

LiterateX can also be used in the publishing of blog entries, magazine
articles, or even books.  Use the [command-line utility](#cli) or integrate
the Haskell [library](#library) into your own software.

LiterateX has support for **source code rules**, comment lines that are used
to visually separate sections of source code.  Since source code rules are
just used to make the source code easier to scan quickly, they are ignored
(treated as a blank line) when translating to Markdown.

LiterateX also has support for [shebang][] lines at the start of the file.
They can be ignored so that they do not precede the documentation.

[shebang]: <https://en.wikipedia.org/wiki/Shebang_(Unix)>

## Source Formats

LiterateX supports a number of source formats.  With the exception of
[literate Haskell](#literate-haskell), documentation as line comments in the
source language.  Note that multi-line comments are treated as code, not
documentation.

### Double-Dash Comments

Documentation is parsed from lines that begin with two dashes immediately
followed by a space (`-- `).  Lines that only contain two dashes (`--`) are
treated as blank lines in the documentation.  Lines that contain three or more
dashes are treated as source code rules and are ignored.

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

* [Elm](https://elm-lang.org/)
* [Haskell](https://www.haskell.org/)
* [Idris](https://www.idris-lang.org/)
* [Lua](http://www.lua.org/)
* [SQL](https://en.wikipedia.org/wiki/SQL)

### Double-Slash Comments

Documentation is parsed from lines that begin with two slashes immediately
followed by a space (`// `).  Lines that only contain two slashes (`//`) are
treated as blank lines in the documentation.  Lines that contain three or more
slashes are treated as source code rules and are ignored.

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
* [CSS](https://en.wikipedia.org/wiki/CSS)
* [Go](https://golang.org/)
* [Java](https://www.java.com/)
* [JavaScript](https://en.wikipedia.org/wiki/JavaScript)
* [Kotlin](https://kotlinlang.org/)
* [PHP](https://www.php.net/)
* [Rust](https://www.rust-lang.org/)
* [Scala](https://www.scala-lang.org/)
* [TypeScript](https://www.typescriptlang.org/)

[C]: <https://en.wikipedia.org/wiki/C_(programming_language)>

### Hash Comments

Documentation is parsed from lines that begin with a hash character
immediately followed by a space (`# `).  Lines that only contain a hash
character (`#`) are treated as blank lines in the documentation.  Lines that
contain two or more hash characters are treated as source code rules and are
ignored.

``` python
# # Python Example
#
# A quick-and-dirty Python script can include commands at the top level!

print("Hello!")

# This simple example just prints "Hello!" to the screen.
```

Languages that use hash comments include the following:

* [Bash](https://www.gnu.org/software/bash/)
* [Elixir](https://elixir-lang.org/)
* [Perl](https://www.perl.org/)
* [Python](https://www.python.org/)
* [R](https://www.r-project.org/)
* [Ruby](https://www.ruby-lang.org/)

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

* [Clojure](https://clojure.org/)
* [Common Lisp](https://common-lisp.net/)
* [Racket](https://racket-lang.org/)
* [Scheme][]

[Scheme]: <https://en.wikipedia.org/wiki/Scheme_(programming_language)>

### Literate Haskell

[GHC](https://www.haskell.org/ghc/) has special support for
[literate programming](https://wiki.haskell.org/Literate_programming).  With
this source format, documentation is not written in comments.  Instead, lines
that contain source code are prefixes with a greater-than sign and a space
(`> `).

Note that this source format does not support source code rules.

``` haskell
# Literate Haskell Example

Executables are implemented using a `Main` module that exposes a function
named `main`.

> module Main (main) where

The `main` function is run when the program is executed.

> main :: IO ()
> main = putStrLn "Hello!"

This simple example just prints "Hello!" to the screen.
```

### Percent Comments

Documentation is parsed from lines that begin with a percent character
immediately followed by a space (`% `).  Lines that only contain a percent
character (`%`) are treated as blank lines in the documentation.  Lines that
contain two or more percent characters are treated as source code rules and
are ignored.

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

* [Erlang](https://www.erlang.org/)
* [LaTeX](https://www.latex-project.org/)

## CLI

LiterateX may be used via a command-line utility named `literatex`.

### Requirements

`literatex` has only been tested on Linux.  It *might* work on Windows and
macOS.

### Installation

#### Installation From Source

`literatex` can be built from source using [Stack][].  For example, you can
install the latest release (to `~/.local/bin` on Linux) as follows:

```
$ git clone https://github.com/ExtremaIS/literatex-haskell.git
$ cd literatex-haskell
$ stack install
```

[Stack]: <https://www.haskellstack.org>

### Usage

See the [`literatex` man page](doc/literatex.1.md) for usage information.

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
whatever order is required by the language.  Those interested being free from
order are encouraged to check out [noweb][] and [CWEB][].

[Literate programming]: <https://en.wikipedia.org/wiki/Literate_programming>
[Donald Knuth]: <https://en.wikipedia.org/wiki/Donald_Knuth>
[the main idea]: <https://www-cs-faculty.stanford.edu/~knuth/cweb.html>
[noweb]: <https://en.wikipedia.org/wiki/Noweb>
[CWEB]: <https://en.wikipedia.org/wiki/CWEB>

The [lhs2tex][] utility is used to work with literate Haskell and LaTeX.

[lhs2tex]: <https://github.com/kosmikus/lhs2tex>

The [src2md][] utility, written in Common Lisp, also supports multiple source
formats.  It outputs Markdown that includes HTML, which limits the usefulness
of the Markdown.

[src2md]: <https://git.sr.ht/~aerique/src2markup>

The [extract-documentation-comments][] utility, written in JavaScript,
extracts documentation from multi-line JavaScript comments.

[extract-documentation-comments]: <https://github.com/Anadian/extract-documentation-comments#readme>

[mlp.clj][], written in Clojure, is a [babashka][] script that transforms
literate Clojure source code to Markdown, including HTML.  The author uses it
to implement a [live preview][] of literate Clojure documentation while using
the [Notepad++][] (Windows editor).

[mlp.clj]: <https://github.com/linpengcheng/ClojureBoxNpp/blob/master/Notepad%2B%2B/tools/clj/mlp.clj>
[babashka]: <https://github.com/babashka/babashka#readme>
[live preview]: <https://github.com/linpengcheng/ClojureBoxNpp/tree/master/Notepad%2B%2B/plugins/Config/PreviewHTML>
[Notepad++]: <https://notepad-plus-plus.org/>

## Project

### Links

* GitHub: <https://github.com/ExtremaIS/literatex-haskell>

### Releases

All releases are tagged in the `main` branch.  Release tags are signed using
the
[`security@extrema.is` GPG key](http://keys.gnupg.net/pks/lookup?op=vindex&fingerprint=on&search=0x1D484E4B4705FADF).

### Contribution

Issues and feature requests are tracked on GitHub:
<https://github.com/ExtremaIS/literatex-haskell/issues>

Issues may also be submitted via email to <bugs@extrema.is>.

### License

This project is released under the
[MIT License](https://opensource.org/licenses/MIT) as specified in the
[`LICENSE`](LICENSE) file.
