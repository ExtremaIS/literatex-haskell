------------------------------------------------------------------------------
-- |
-- Module      : LiterateX.Parser
-- Description : source parser
-- Copyright   : Copyright (c) 2021-2022 Travis Cardwell
-- License     : MIT
--
-- This module implements the source parser.
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module LiterateX.Parser
  ( -- * API
    parse
  ) where

-- https://hackage.haskell.org/package/base
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

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
parse = parseSourceLines . parserFunctionsFor

------------------------------------------------------------------------------
-- $Internal

-- | Parser functions that determine how input is parsed
data ParserFunctions
  = ParserFunctions
    { isCodeBlank :: !(Text -> Bool)
    , isDocBlank  :: !(Text -> Bool)
    , isRule      :: !(Text -> Bool)
    , getDoc      :: !(Text -> Maybe Text)
    , getCode     :: !(Text -> Text)
    }

------------------------------------------------------------------------------

-- | Get the parser functions for the specified source format
parserFunctionsFor :: SourceFormat -> ParserFunctions
parserFunctionsFor = \case
    SourceFormat.DoubleDash      -> lineCommentParserFunctions '-' 2
    SourceFormat.DoubleSlash     -> lineCommentParserFunctions '/' 2
    SourceFormat.Hash            -> lineCommentParserFunctions '#' 1
    SourceFormat.LiterateHaskell -> literateHaskellParserFunctions
    SourceFormat.Percent         -> lineCommentParserFunctions '%' 1
    SourceFormat.LispSemicolons  -> lispCommentParserFunctions

------------------------------------------------------------------------------

-- | Get parser functions for source with line-based comments
lineCommentParserFunctions
  :: Char  -- ^ comment character
  -> Int   -- ^ number of comment characters to create line comment
  -> ParserFunctions
lineCommentParserFunctions char count = ParserFunctions{..}
  where
    docBlank :: Text
    docBlank = T.pack $ replicate count char

    prefixLen :: Int
    prefixLen = count + 1

    prefix :: Text
    prefix = T.pack $ replicate count char ++ " "

    isCodeBlank :: Text -> Bool
    isCodeBlank = T.null

    isDocBlank :: Text -> Bool
    isDocBlank = (== docBlank)

    isRule :: Text -> Bool
    isRule line = T.length line > count && T.all (== char) line

    getDoc :: Text -> Maybe Text
    getDoc line = do
      let (linePrefix, lineSuffix) = T.splitAt prefixLen line
      guard $ linePrefix == prefix
      pure lineSuffix

    getCode :: Text -> Text
    getCode = id

------------------------------------------------------------------------------

-- | Get parser functions for source with Lisp-style comments
--
-- Lisp-style comments begin with one or more semicolons.
lispCommentParserFunctions :: ParserFunctions
lispCommentParserFunctions = ParserFunctions{..}
  where
    isCodeBlank :: Text -> Bool
    isCodeBlank = T.null

    isDocBlank :: Text -> Bool
    isDocBlank line =
      let len = T.length line
      in  len >= 1 && len <= 4 && T.all (== ';') line

    isRule :: Text -> Bool
    isRule line = T.length line > 4 && T.all (== ';') line

    getDoc :: Text -> Maybe Text
    getDoc line = do
      let (linePrefix, (sep, lineSuffix)) = T.splitAt 1 <$> T.breakOn " " line
      guard $ not (T.null linePrefix) && T.all (== ';') linePrefix
      guard $ sep == " "
      pure lineSuffix

    getCode :: Text -> Text
    getCode = id

------------------------------------------------------------------------------

-- | Get parser functions for parsing literate Haskell
literateHaskellParserFunctions :: ParserFunctions
literateHaskellParserFunctions = ParserFunctions{..}
  where
    isCodeBlank :: Text -> Bool
    isCodeBlank = (== ">")

    isDocBlank :: Text -> Bool
    isDocBlank = T.null

    isRule :: Text -> Bool
    isRule = const False

    getDoc :: Text -> Maybe Text
    getDoc line
      | "> " `T.isPrefixOf` line = Nothing
      | otherwise                = Just line

    getCode :: Text -> Text
    getCode line = fromMaybe line $ T.stripPrefix "> " line

------------------------------------------------------------------------------

-- | Create a "Conduit" transformer for the specified parser functions
--
-- This function produces a 'SourceLine' for each line of input.  A
-- 'SourceLine.Shebang' can only be produced on the first line.  Note that the
-- order that the parser functions are used is significant; the parser
-- functions are written for this order.
parseSourceLines
  :: Monad m
  => ParserFunctions
  -> ConduitT Text SourceLine m ()
parseSourceLines ParserFunctions{..} = do
    mLine <- C.await
    case mLine of
      Just line -> do
        C.yield $ if "#!" `T.isPrefixOf` line
          then SourceLine.Shebang line
          else parse' line
        C.awaitForever $ C.yield . parse'
      Nothing -> return ()
  where
    parse' :: Text -> SourceLine
    parse' line
      | isCodeBlank line = SourceLine.CodeBlank
      | isDocBlank line  = SourceLine.DocBlank
      | isRule line      = SourceLine.Rule
      | otherwise        = case (getDoc line, getCode line) of
          (Just doc, _code) -> SourceLine.Doc doc
          (Nothing,  code)  -> SourceLine.Code code
