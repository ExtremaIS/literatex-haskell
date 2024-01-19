------------------------------------------------------------------------------
-- |
-- Module      : LiterateX.Types.TargetFormat
-- Description : target format type
-- Copyright   : Copyright (c) 2021-2024 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module LiterateX.Types.TargetFormat
  ( -- * Type
    TargetFormat(..)
    -- * API
  , describe
  , list
  , mkEndCode
  , mkBeginCode
  ) where

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

-- (literatex)
import LiterateX.Types.CodeLanguage (CodeLanguage)

------------------------------------------------------------------------------
-- $Type

-- | Target format
--
-- This sum type defines the supported target formats.
--
-- Documentation:
--
-- * [Pandoc Markdown](https://pandoc.org/MANUAL.html#pandocs-markdown)
-- * [GitHub Flavored Markdown](https://github.github.com/gfm/)
-- * [mdBook Markdown](https://rust-lang.github.io/mdBook/)
--
-- @since 0.2.1.0
data TargetFormat
  = PandocMarkdown
  | GitHubFlavoredMarkdown
  | MdBook
  deriving (Bounded, Enum, Eq, Ord, Show)

instance TTC.Parse TargetFormat where
  parse = TTC.parseEnum' "target format" True False

instance TTC.Render TargetFormat where
  render = TTC.fromS . \case
    PandocMarkdown         -> "pandoc"
    GitHubFlavoredMarkdown -> "github"
    MdBook                 -> "mdbook"

------------------------------------------------------------------------------
-- $API

-- | Get a description of a target format
--
-- @since 0.0.1.0
describe :: TargetFormat -> String
describe = \case
    PandocMarkdown         -> "Pandoc Markdown"
    GitHubFlavoredMarkdown -> "GitHub Flavored Markdown"
    MdBook                 -> "mdBook Markdown"

------------------------------------------------------------------------------

-- | List of all supported target formats
--
-- @since 0.0.1.0
list :: [TargetFormat]
list = [minBound ..]

------------------------------------------------------------------------------

-- | Make line in the target format to end a block of source code
--
-- @since 0.0.1.0
mkEndCode
  :: TargetFormat
  -> Text
mkEndCode _anyFormat = "```"

------------------------------------------------------------------------------

-- | Make line in the target format to begin a block of code
--
-- This function is written to indicate how it is used.  Given the target
-- format, optional code language, and line numbering flag, this function
-- returns a function that takes a line number and returns a line.
--
-- The 'MdBook' format does not support per-block line numbering, so the line
-- numbering flag is ignored for that format.
--
-- @since 0.0.1.0
mkBeginCode
  :: TargetFormat
  -> Maybe CodeLanguage
  -> Bool           -- ^ 'True' to number code lines
  -> (Int -> Text)  -- ^ make line for code starting at specified line number
mkBeginCode PandocMarkdown (Just lang) True = \lineNum -> T.concat
    [ "``` {.", TTC.render lang, " .numberSource startFrom=\""
    , TTC.renderWithShow lineNum, "\"}"
    ]
mkBeginCode PandocMarkdown (Just lang) False = const $ T.concat
    [ "```", TTC.render lang
    ]
mkBeginCode PandocMarkdown Nothing True = \lineNum -> T.concat
    [ "``` {.numberSource startFrom=\"", TTC.renderWithShow lineNum, "\"}"
    ]
mkBeginCode GitHubFlavoredMarkdown (Just lang) True = \lineNum -> T.concat
    [ "``` ", TTC.render lang, " startline=", TTC.renderWithShow lineNum
    ]
mkBeginCode GitHubFlavoredMarkdown (Just lang) False = const $ T.concat
    [ "``` ", TTC.render lang
    ]
mkBeginCode GitHubFlavoredMarkdown Nothing True = \lineNum -> T.concat
    [ "``` startline=", TTC.renderWithShow lineNum
    ]
mkBeginCode MdBook (Just lang) _isNumbered = const $ T.concat
    [ "```", TTC.render lang
    ]
mkBeginCode _anyFormat Nothing _isNumbered = const "```"
