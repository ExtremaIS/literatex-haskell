------------------------------------------------------------------------------
-- |
-- Module      : LiterateX.Renderer
-- Description : target renderer
-- Copyright   : Copyright (c) 2021-2022 Travis Cardwell
-- License     : MIT
--
-- This module implements the target renderer.
------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LiterateX.Renderer
  ( -- * Types
    Options(..)
    -- * API
  , defaultOptions
  , defaultOptionsFor
  , render
  ) where

-- https://hackage.haskell.org/package/base
import Control.Monad (replicateM_)

-- https://hackage.haskell.org/package/conduit
import qualified Data.Conduit as C
import Data.Conduit (ConduitT)

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)

-- (literatex)
import LiterateX.Types (CodeLanguage, SourceLine, TargetFormat)
import qualified LiterateX.Types.SourceLine as SourceLine
import qualified LiterateX.Types.TargetFormat as TargetFormat

------------------------------------------------------------------------------
-- $Types

-- | Renderer options determine how output is rendered
--
-- @since 0.0.1.0
data Options
  = Options
    { targetFormat    :: !TargetFormat
    , codeLanguage    :: !(Maybe CodeLanguage)
    , ignoreShebang   :: !Bool  -- ^ 'True' to ignore shebangs
    , renderCode      :: !Bool  -- ^ 'True' to render code
    , numberCodeLines :: !Bool  -- ^ 'True' to number code lines
    }
  deriving Show

------------------------------------------------------------------------------
-- $API

-- | Default options
--
-- * @targetFormat@: 'TargetFormat.PandocMarkdown'
-- * @codeLanguage@: 'Nothing'
-- * @ignoreShebang@: 'True'
-- * @renderCode@: 'True'
-- * @numberCodeLines@: 'True'
--
-- @since 0.0.1.0
defaultOptions :: Options
defaultOptions = Options
    { targetFormat    = TargetFormat.PandocMarkdown
    , codeLanguage    = Nothing
    , ignoreShebang   = True
    , renderCode      = True
    , numberCodeLines = True
    }

-- | Default options for the specified code language
--
-- @since 0.0.1.0
defaultOptionsFor :: CodeLanguage -> Options
defaultOptionsFor lang = defaultOptions
    { codeLanguage = Just lang
    }

-- | Create a "Conduit" transformer that renders using the specified options
--
-- The transformer consumes 'SourceLine' values annotated with the line number
-- and produces lines of output.
--
-- @since 0.0.1.0
render
  :: forall m. Monad m
  => Options
  -> ConduitT (Int, SourceLine) Text m ()
render Options{..} = C.await >>= sStart
  where
    -- Start state
    sStart
      :: Maybe (Int, SourceLine)
      -> ConduitT (Int, SourceLine) Text m ()
    sStart (Just (lineNum, sourceLine)) = case sourceLine of
      -- render shebang if enabled, otherwise skip
      SourceLine.Shebang line
        | renderCode && not ignoreShebang -> do
            C.yield $ startShebang lineNum
            C.yield line
            C.yield endCode
            C.await >>= sBlank
        | otherwise -> C.await >>= sStart
      -- start rendering documentation
      SourceLine.Doc line -> do
        C.yield line
        C.await >>= sDoc
      -- start rendering code if enabled, otherwise skip
      SourceLine.Code line
        | renderCode -> do
            C.yield $ startCode lineNum
            C.yield line
            C.await >>= sCode 0
        | otherwise -> C.await >>= sStart
      -- skip blank lines and rules
      _BlankOrRule -> C.await >>= sStart
    sStart Nothing = return ()

    -- Code state
    sCode
      :: Int  -- ^ number of pending (code) blank lines
      -> Maybe (Int, SourceLine)
      -> ConduitT (Int, SourceLine) Text m ()
    sCode !numBlanks (Just (_lineNum, sourceLine)) = case sourceLine of
      -- render any pending blank lines and code
      SourceLine.Code line -> do
        replicateM_ numBlanks $ C.yield T.empty
        C.yield line
        C.await >>= sCode 0
      -- increment number of pending blank lines
      SourceLine.CodeBlank -> C.await >>= sCode (numBlanks + 1)
      -- end code, add blank line, start rendering documentation
      SourceLine.Doc line -> do
        C.yield endCode
        C.yield T.empty
        C.yield line
        C.await >>= sDoc
      -- end code, enter blank state
      SourceLine.DocBlank -> do
        C.yield endCode
        C.await >>= sBlank
      -- end code, enter blank state
      SourceLine.Rule -> do
        C.yield endCode
        C.await >>= sBlank
      -- shebang only possible on first line (seen in start state)
      SourceLine.Shebang _line -> error "impossible: Shebang in sCode"
    sCode _numBlanks Nothing = C.yield endCode

    -- Documentation state
    sDoc
      :: Maybe (Int, SourceLine)
      -> ConduitT (Int, SourceLine) Text m ()
    sDoc (Just (lineNum, sourceLine)) = case sourceLine of
      -- render documentation
      SourceLine.Doc line -> do
        C.yield line
        C.await >>= sDoc
      -- start rendering code if enabled, otherwise enter blank state
      SourceLine.Code line
        | renderCode -> do
            C.yield T.empty
            C.yield $ startCode lineNum
            C.yield line
            C.await >>= sCode 0
        | otherwise -> C.await >>= sBlank
      -- shebang only possible on first line (seen in start state)
      SourceLine.Shebang _line -> error "impossible: Shebang in sDoc"
      -- blank lines and rules transition to blank state
      _BlankOrRule -> C.await >>= sBlank
    sDoc Nothing = return ()

    -- Blank state
    sBlank
      :: Maybe (Int, SourceLine)
      -> ConduitT (Int, SourceLine) Text m ()
    sBlank (Just (lineNum, sourceLine)) = case sourceLine of
      -- start rendering code if enabled, otherwise skip
      SourceLine.Code line
        | renderCode -> do
            C.yield T.empty
            C.yield $ startCode lineNum
            C.yield line
            C.await >>= sCode 0
        | otherwise -> C.await >>= sBlank
      -- render blank line, start rendering documentation
      SourceLine.Doc line -> do
        C.yield T.empty
        C.yield line
        C.await >>= sDoc
      -- shebang only possible on first line (seen in start state)
      SourceLine.Shebang _line  -> error "impossible: Shebang in sBlank"
      -- skip additional blank lines and rules
      _BlankOrRule -> C.await >>= sBlank
    sBlank Nothing = return ()

    -- start code line for code starting at given line number
    startCode :: Int -> Text
    startCode =
      TargetFormat.mkBeginCode targetFormat codeLanguage numberCodeLines

    -- shebang start code line does not use syntax highlighting
    startShebang :: Int -> Text
    startShebang =
      TargetFormat.mkBeginCode targetFormat Nothing numberCodeLines

    -- end code line
    endCode :: Text
    endCode = TargetFormat.mkEndCode targetFormat
{-# ANN render ("HLint: ignore Reduce duplication" :: String) #-}
