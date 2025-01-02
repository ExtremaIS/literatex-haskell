------------------------------------------------------------------------------
-- |
-- Module      : LiterateX.Parser
-- Description : source parser
-- Copyright   : Copyright (c) 2021-2025 Travis Cardwell
-- License     : MIT
--
-- This module implements the source parser.
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module LiterateX.Parser
  ( -- * API
    parse
  ) where

-- https://hackage.haskell.org/package/conduit
import qualified Data.Conduit as C
import Data.Conduit (ConduitT)

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)

-- (literatex)
import LiterateX.Types (SourceFormat, SourceLine)
import qualified LiterateX.Types.SourceFormat as SourceFormat
import qualified LiterateX.Types.SourceLine as SourceLine

------------------------------------------------------------------------------
-- $API

-- | Create a "Conduit" transformer that parses the specified source format
--
-- The transformer consumes lines of the input and produces a 'SourceLine' for
-- each line of input.
--
-- @since 0.0.1.0
parse
  :: Monad m
  => SourceFormat
  -> ConduitT Text SourceLine m ()
parse sourceFormat = do
    let parseLine' = parseLine sourceFormat
    mLine <- C.await
    case mLine of
      Just line -> do
        C.yield $ if "#!" `T.isPrefixOf` line
          then SourceLine.Shebang line
          else parseLine' line
        C.awaitForever $ C.yield . parseLine'
      Nothing -> return ()

------------------------------------------------------------------------------
-- $Internal

-- | Parse a source line according to the source format
parseLine :: SourceFormat -> Text -> SourceLine
parseLine = \case
    SourceFormat.DoubleDash      -> parseLineCommentLine '-' 2
    SourceFormat.DoubleSlash     -> parseLineCommentLine '/' 2
    SourceFormat.Hash            -> parseLineCommentLine '#' 1
    SourceFormat.LiterateHaskell -> parseLiterateHaskellLine
    SourceFormat.Percent         -> parseLineCommentLine '%' 1
    SourceFormat.LispSemicolons  -> parseLispCommentLine

------------------------------------------------------------------------------

-- | Parse a source line using line-based comments
parseLineCommentLine
  :: Char  -- ^ comment character
  -> Int   -- ^ number of comment characters to create line comment
  -> Text  -- ^ source line
  -> SourceLine
parseLineCommentLine char count line
    | T.null line = SourceLine.CodeBlank
    | otherwise = case T.uncons <$> T.span (== char) line of
        ("", _) -> SourceLine.Code line
        (_, Nothing) -> case T.compareLength line count of
          EQ -> SourceLine.DocBlank
          GT -> SourceLine.Rule
          LT -> SourceLine.Code line
        (l, Just (' ', r)) | T.compareLength l count == EQ -> SourceLine.Doc r
        _otherwise -> SourceLine.Code line

------------------------------------------------------------------------------

-- | Parse a source line using Lisp-style comments
--
-- Lisp-style comments begin with one or more semicolons.
parseLispCommentLine
  :: Text  -- ^ source line
  -> SourceLine
parseLispCommentLine line
    | T.null line = SourceLine.CodeBlank
    | otherwise = case T.uncons <$> T.span (== ';') line of
        ("", _) -> SourceLine.Code line
        (_, Nothing)
          | T.compareLength line 4 == GT -> SourceLine.Rule
          | otherwise -> SourceLine.DocBlank
        (_, Just (' ', r)) -> SourceLine.Doc r
        _otherwise -> SourceLine.Code line

------------------------------------------------------------------------------

-- | Parse a Literate Haskell source line
parseLiterateHaskellLine
  :: Text  -- ^ source line
  -> SourceLine
parseLiterateHaskellLine line = case T.uncons line of
    Nothing -> SourceLine.DocBlank
    Just ('>', r1) -> case T.uncons r1 of
      Nothing -> SourceLine.CodeBlank
      Just (' ', r2) -> SourceLine.Code r2
      _otherwise -> SourceLine.Doc line
    _otherwise -> SourceLine.Doc line
