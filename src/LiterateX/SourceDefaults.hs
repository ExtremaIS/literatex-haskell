------------------------------------------------------------------------------
-- |
-- Module      : LiterateX.SourceDefaults
-- Description : default options by source extension
-- Copyright   : Copyright (c) 2021 Travis Cardwell
-- License     : MIT
--
-- This module provides some default options for various sources.
------------------------------------------------------------------------------

module LiterateX.SourceDefaults
  ( -- * API
    defaultsFor
  , extensionDefaults
  ) where

-- https://hackage.haskell.org/package/base
import Data.List (find, isSuffixOf)

-- (literatex)
import LiterateX.Types (CodeLanguage, SourceFormat)
import qualified LiterateX.Types.SourceFormat as SourceFormat

------------------------------------------------------------------------------
-- $API

-- | Get the default source format and code language for the given filename
--
-- @since 0.0.1.0
defaultsFor :: FilePath -> Maybe (SourceFormat, CodeLanguage)
defaultsFor path =
  fmap snd . flip find extensionDefaults $ flip isSuffixOf path . fst

------------------------------------------------------------------------------

-- | List of default options for various filename extensions
--
-- @since 0.0.1.0
extensionDefaults :: [(String, (SourceFormat, CodeLanguage))]
extensionDefaults =
    [ (".c",    (SourceFormat.DoubleSlash,     "c"))
    , (".clj",  (SourceFormat.LispSemicolons,  "clojure"))
    , (".css",  (SourceFormat.DoubleSlash,     "css"))
    , (".elm",  (SourceFormat.DoubleDash,      "elm"))
    , (".erl",  (SourceFormat.Percent,         "erlang"))
    , (".ex",   (SourceFormat.Hash,            "elixir"))
    , (".exs",  (SourceFormat.Hash,            "elixir"))
    , (".go",   (SourceFormat.DoubleSlash,     "go"))
    , (".hs",   (SourceFormat.DoubleDash,      "haskell"))
    , (".idr",  (SourceFormat.DoubleDash,      "idris"))
    , (".java", (SourceFormat.DoubleSlash,     "java"))
    , (".js",   (SourceFormat.DoubleSlash,     "javascript"))
    , (".kt",   (SourceFormat.DoubleSlash,     "kotlin"))
    , (".lhs",  (SourceFormat.LiterateHaskell, "haskell"))
    , (".lisp", (SourceFormat.LispSemicolons,  "commonlisp"))
    , (".lua",  (SourceFormat.DoubleDash,      "lua"))
    , (".php",  (SourceFormat.DoubleSlash,     "php"))
    , (".pl",   (SourceFormat.Hash,            "perl"))
    , (".py",   (SourceFormat.Hash,            "python"))
    , (".r",    (SourceFormat.Hash,            "r"))
    , (".rb",   (SourceFormat.Hash,            "ruby"))
    , (".rkt",  (SourceFormat.LispSemicolons,  "scheme"))
    , (".rs",   (SourceFormat.DoubleSlash,     "rust"))
    , (".sc",   (SourceFormat.DoubleSlash,     "scala"))
    , (".scm",  (SourceFormat.LispSemicolons,  "scheme"))
    , (".sh",   (SourceFormat.Hash,            "bash"))
    , (".sql",  (SourceFormat.DoubleDash,      "sql"))
    , (".tex",  (SourceFormat.Percent,         "latex"))
    , (".ts",   (SourceFormat.DoubleSlash,     "typescript"))
    ]
