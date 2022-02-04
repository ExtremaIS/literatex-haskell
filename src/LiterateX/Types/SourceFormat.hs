------------------------------------------------------------------------------
-- |
-- Module      : LiterateX.Types.SourceFormat
-- Description : source format type
-- Copyright   : Copyright (c) 2021-2022 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module LiterateX.Types.SourceFormat
  ( -- * Type
    SourceFormat(..)
    -- * API
  , describe
  , list
  ) where

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

------------------------------------------------------------------------------
-- $Type

-- | Source format
--
-- This sum type defines the supported source formats.
--
-- @since 0.0.1.0
data SourceFormat
  = DoubleDash       -- ^ \-- comments
  | DoubleSlash      -- ^ // comments
  | Hash             -- ^ # comments
  | LispSemicolons   -- ^ Lisp semicolon comments
  | LiterateHaskell  -- ^ literate Haskell
  | Percent          -- ^ % comments
  deriving (Bounded, Enum, Eq, Ord, Show)

instance TTC.Parse SourceFormat where
  parse = TTC.parseEnum' "source format" True False

instance TTC.Render SourceFormat where
  render = TTC.fromS . \case
    DoubleDash      -> "ddash"
    DoubleSlash     -> "dslash"
    Hash            -> "hash"
    LispSemicolons  -> "lisp"
    LiterateHaskell -> "lhs"
    Percent         -> "percent"

------------------------------------------------------------------------------
-- $API

-- | Get a description of a source format
--
-- @since 0.0.1.0
describe :: SourceFormat -> String
describe = \case
    DoubleDash      -> "-- comments"
    DoubleSlash     -> "// comments"
    Hash            -> "# comments"
    LispSemicolons  -> "Lisp semicolon comments"
    LiterateHaskell -> "literate Haskell"
    Percent         -> "% comments"

------------------------------------------------------------------------------

-- | List of all supported source formats
--
-- @since 0.0.1.0
list :: [SourceFormat]
list = [minBound ..]
