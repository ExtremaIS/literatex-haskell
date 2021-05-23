------------------------------------------------------------------------------
-- |
-- Module      : LiterateX.Types.CodeLanguage
-- Description : source code language type
-- Copyright   : Copyright (c) 2021 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

module LiterateX.Types.CodeLanguage
  ( -- * Type
    CodeLanguage
  ) where

-- https://hackage.haskell.org/package/base
import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.String (IsString(fromString))

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

------------------------------------------------------------------------------
-- $Type

-- | Source code language
--
-- This string is used for syntax highlighting of source code.
--
-- When using Pandoc Markdown, run the following command to see a list of
-- supported languages:
--
-- @
-- \$ pandoc --list-highlight-languages
-- @
--
-- When using GitHub Flavored Markdown, check the following file for supported
-- languages:
--
-- <https://github.com/github/linguist/blob/master/lib/linguist/languages.yml>
--
-- @since 0.0.1.0
newtype CodeLanguage = CodeLanguage { unCodeLanguage :: Text }
  deriving (Eq, Ord, Show)

instance IsString CodeLanguage where
  fromString = either error id . TTC.parse

instance TTC.Parse CodeLanguage where
  parse = TTC.asT $ \t -> first TTC.fromS $ do
    when (T.null t) $ Left "invalid code language: empty"
    when (T.any isSpace t) $ Left "invalid code language: contains whitespace"
    return $ CodeLanguage t

instance TTC.Render CodeLanguage where
  render = TTC.fromT . unCodeLanguage
