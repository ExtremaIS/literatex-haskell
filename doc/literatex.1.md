---
title: LITERATEX
section: 1
hyphenate: false
...

# NAME

`literatex` - transform literate source code to Markdown

# SYNOPSIS

`literatex` [*OPTIONS*]

# DESCRIPTION

LiterateX transforms literate source code to Markdown.  Write documentation in
Markdown format in the comments of source code, and LiterateX can transform
the file to Markdown, optionally including the source code with syntax
highlighting and line numbers.

# OPTIONS

-h, \--help
:   show help and exit

\--version
:   show version and exit

-s, \--source *SOURCE*
:   source format

    The following source formats are supported:

    * `ddash` - `--` comments
    * `dslash` - `//` comments
    * `hash` - `#` comments
    * `lisp` - Lisp semicolon comments
    * `lhs` - literate Haskell
    * `percent` - `%` comments

-t, \--target *TARGET*
:   target format (default: `pandoc`)

    The following target formats are supported:

    * `pandoc` - Pandoc Markdown
    * `github` - GitHub Flavored Markdown
    * `mdbook` - mdBook Markdown

-l, \--language *LANGUAGE*
:   code language

\--ignore-shebang
:   ignore shebangs

\--no-code
:   do not display code

\--no-numbers
:   do not number code lines

\--no-highlight
:   do not highlight code

-i, \--input *FILE*
:   input file (default: `STDIN`)

-o, \--output *FILE*
:   output file (default: `STDOUT`)

# Default Options

`.c`
:   c (dslash)

`.clj`
:   clojure (lisp)

`.css`
:   css (dslash)

`.elm`
:   elm (ddash)

`.erl`
:   erlang (percent)

`.ex`
:   elixir (hash)

`.exs`
:   elixir (hash)

`.go`
:   go (dslash)

`.hs`
:   haskell (ddash)

`.idr`
:   idris (ddash)

`.java`
:   java (dslash)

`.js`
:   javascript (dslash)

`.kt`
:   kotlin (dslash)

`.lhs`
:   haskell (lhs)

`.lisp`
:   commonlisp (lisp)

`.lua`
:   lua (ddash)

`.php`
:   php (dslash)

`.pl`
:   perl (hash)

`.py`
:   python (hash)

`.r`
:   r (hash)

`.rb`
:   ruby (hash)

`.rkt`
:   scheme (lisp)

`.rs`
:   rust (dslash)

`.sc`
:   scala (dslash)

`.scm`
:   scheme (lisp)

`.sh`
:   bash (hash)

`.sql`
:   sql (ddash)

`.tex`
:   latex (percent)

`.ts`
:   typescript (dslash)

# PROJECT

GitHub:
:   <https://github.com/ExtremaIS/literatex-haskell>

Reporting issues:
:   GitHub: <https://github.com/ExtremaIS/literatex-haskell/issues>

    Email: <bugs@extrema.is>

Copyright
:   Copyright (c) 2021-2024 Travis Cardwell

License
:   The MIT License <https://opensource.org/licenses/MIT>
