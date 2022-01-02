------------------------------------------------------------------------------
-- |
-- Module      : LiterateX
-- Description : API
-- Copyright   : Copyright (c) 2021-2022 Travis Cardwell
-- License     : MIT
--
-- This module provides high-level as well as low-level API functions for
-- transforming literate source code.
------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module LiterateX
  ( -- * Constants
    version
    -- * API
    -- ** High-Level
    -- $HighLevelAPI
  , transformTextToText
  , transformTextToHandle
  , transformTextToFile
  , transformHandleToText
  , transformHandleToHandle
  , transformHandleToFile
  , transformFileToText
  , transformFileToHandle
  , transformFileToFile
    -- ** Low-Level
    -- $LowLevelAPI
  , runPure
  , runIO
  , runResource
    -- *** Producers
  , sourceString
  , sourceText
  , sourceLazyText
  , sourceByteString
  , sourceLazyByteString
  , sourceHandle
  , sourceFile
    -- *** Consumers
  , sinkString
  , sinkText
  , sinkLazyText
  , sinkByteString
  , sinkLazyByteString
  , sinkHandle
  , sinkFile
    -- *** Transformers
  , transform
  ) where

-- https://hackage.haskell.org/package/base
import Control.Monad.IO.Class (MonadIO)
import Data.Version (showVersion)
import System.IO (Handle)

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- https://hackage.haskell.org/package/conduit
import qualified Conduit as C
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Lazy as TL

-- https://hackage.haskell.org/package/unliftio
import UnliftIO (MonadUnliftIO)

-- (literatex)
import qualified LiterateX.Parser as Parser
import qualified LiterateX.Renderer as Renderer
import LiterateX.Types (SourceFormat)

-- (literatex:cabal)
import qualified Paths_literatex as Project

------------------------------------------------------------------------------
-- $Constants

-- | LiterateX version string (\"@literatex-haskell X.X.X@\")
version :: String
version = "literatex-haskell " ++ showVersion Project.version

------------------------------------------------------------------------------
-- $HighLevelAPI
--
-- This high-level API provides functions for transforming literate source
-- code.  These functions provide support for transforming from/to lazy
-- 'TL.Text', 'Handle's, and files.
--
-- Example usage:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
--
-- module Main (main) where
--
-- -- https://hackage.haskell.org/package/literatex
-- import qualified LiterateX
-- import qualified LiterateX.Renderer as Renderer
-- import qualified LiterateX.Types.SourceFormat as SourceFormat
--
-- main :: IO ()
-- main = LiterateX.transformFileToFile
--     SourceFormat.LiterateHaskell
--     (Renderer.defaultOptionsFor "haskell")
--     "demo.lhs"
--     "demo.md"
-- @

-- | Transform from lazy 'TL.Text' to lazy 'TL.Text'
--
-- @since 0.0.1.0
transformTextToText
  :: SourceFormat
  -> Renderer.Options
  -> TL.Text
  -> TL.Text
transformTextToText sourceFormat rendererOpts source =
    runPure sourceFormat rendererOpts (sourceLazyText source) sinkLazyText

-- | Transform from lazy 'TL.Text' to a 'Handle'
--
-- @since 0.0.1.0
transformTextToHandle
  :: MonadIO m
  => SourceFormat
  -> Renderer.Options
  -> TL.Text
  -> Handle
  -> m ()
transformTextToHandle sourceFormat rendererOpts source target =
    runIO sourceFormat rendererOpts
      (sourceLazyText source)
      (sinkHandle target)

-- | Transform from lazy 'TL.Text' to a file
--
-- @since 0.0.1.0
transformTextToFile
  :: MonadUnliftIO m
  => SourceFormat
  -> Renderer.Options
  -> TL.Text
  -> FilePath
  -> m ()
transformTextToFile sourceFormat rendererOpts source target =
    runResource sourceFormat rendererOpts
      (sourceLazyText source)
      (sinkFile target)

-- | Transform from a 'Handle' to lazy 'TL.Text'
--
-- @since 0.0.1.0
transformHandleToText
  :: MonadIO m
  => SourceFormat
  -> Renderer.Options
  -> Handle
  -> m TL.Text
transformHandleToText sourceFormat rendererOpts source =
    runIO sourceFormat rendererOpts (sourceHandle source) sinkLazyText

-- | Transform from a 'Handle' to a 'Handle'
--
-- @since 0.0.1.0
transformHandleToHandle
  :: MonadIO m
  => SourceFormat
  -> Renderer.Options
  -> Handle
  -> Handle
  -> m ()
transformHandleToHandle sourceFormat rendererOpts source target =
    runIO sourceFormat rendererOpts (sourceHandle source) (sinkHandle target)

-- | Transform from a 'Handle' to a file
--
-- @since 0.0.1.0
transformHandleToFile
  :: MonadUnliftIO m
  => SourceFormat
  -> Renderer.Options
  -> Handle
  -> FilePath
  -> m ()
transformHandleToFile sourceFormat rendererOpts source target =
    runResource sourceFormat rendererOpts
      (sourceHandle source)
      (sinkFile target)

-- | Transform from a file to lazy 'TL.Text'
--
-- @since 0.0.1.0
transformFileToText
  :: MonadUnliftIO m
  => SourceFormat
  -> Renderer.Options
  -> FilePath
  -> m TL.Text
transformFileToText sourceFormat rendererOpts source =
    runResource sourceFormat rendererOpts (sourceFile source) sinkLazyText

-- | Transform from a file to a 'Handle'
--
-- @since 0.0.1.0
transformFileToHandle
  :: MonadUnliftIO m
  => SourceFormat
  -> Renderer.Options
  -> FilePath
  -> Handle
  -> m ()
transformFileToHandle sourceFormat rendererOpts source target =
    runResource sourceFormat rendererOpts
      (sourceFile source)
      (sinkHandle target)

-- | Transform from a file to a file
--
-- @since 0.0.1.0
transformFileToFile
  :: MonadUnliftIO m
  => SourceFormat
  -> Renderer.Options
  -> FilePath
  -> FilePath
  -> m ()
transformFileToFile sourceFormat rendererOpts source target =
    runResource sourceFormat rendererOpts
      (sourceFile source)
      (sinkFile target)

------------------------------------------------------------------------------
-- $LowLevelAPI
--
-- This low-level API provides more control over transforming literate source
-- code, using "Conduit".  The 'transform' transformer implements the
-- transformation, transforming lines of input 'T.Text' to lines of output
-- 'T.Text'.  Various producers are provided to produce lines of input
-- 'T.Text' from common sources, and various consumers are provided to consume
-- lines of output 'T.Text' to common sinks.  These can be used separately if
-- necessary, but some run functions are provided for common usage.
--
-- Example usage:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
--
-- module Main (main) where
--
-- -- https://hackage.haskell.org/package/base
-- import System.IO (stdout)
--
-- -- https://hackage.haskell.org/package/literatex
-- import qualified LiterateX
-- import qualified LiterateX.Renderer as Renderer
-- import qualified LiterateX.Types.SourceFormat as SourceFormat
--
-- main :: IO ()
-- main = do
--     let demoBS = "..."
--     LiterateX.runIO
--       SourceFormat.LiterateHaskell
--       (Renderer.defaultOptionsFor "haskell")
--       (LiterateX.sourceByteString demoBS)
--       (LiterateX.sinkHandle stdout)
-- @

-- | Run a pure LiterateX transformation
--
-- This function works with the following input line producers:
--
-- * 'sourceString'
-- * 'sourceText'
-- * 'sourceLazyText'
-- * 'sourceByteString'
-- * 'sourceLazyByteString'
--
-- This function works with the following output line consumers:
--
-- * 'sinkString'
-- * 'sinkText'
-- * 'sinkLazyText'
-- * 'sinkByteString'
-- * 'sinkLazyByteString'
--
-- @since 0.0.1.0
runPure
  :: SourceFormat
  -> Renderer.Options
  -> C.ConduitT () T.Text C.Identity ()     -- ^ input line producer
  -> C.ConduitT T.Text C.Void C.Identity r  -- ^ output line consumer
  -> r
runPure sourceFormat rendererOpts source sink = C.runConduitPure $
    source .| transform sourceFormat rendererOpts .| sink

-- | Run a LiterateX transformation using IO
--
-- This function works with the following input line producers:
--
-- * 'sourceString'
-- * 'sourceText'
-- * 'sourceLazyText'
-- * 'sourceByteString'
-- * 'sourceLazyByteString'
-- * 'sourceHandle'
--
-- This function works with the following output line consumers:
--
-- * 'sinkString'
-- * 'sinkText'
-- * 'sinkLazyText'
-- * 'sinkByteString'
-- * 'sinkLazyByteString'
-- * 'sinkHandle'
--
-- @since 0.0.1.0
runIO
  :: MonadIO m
  => SourceFormat
  -> Renderer.Options
  -> C.ConduitT () T.Text m ()     -- ^ input line producer
  -> C.ConduitT T.Text C.Void m r  -- ^ output line consumer
  -> m r
runIO sourceFormat rendererOpts source sink = C.runConduit $
    source .| transform sourceFormat rendererOpts .| sink

-- | Run a LiterateX transformation using resource management
--
-- This function works with the following input line producers:
--
-- * 'sourceString'
-- * 'sourceText'
-- * 'sourceLazyText'
-- * 'sourceByteString'
-- * 'sourceLazyByteString'
-- * 'sourceHandle'
-- * 'sourceFile'
--
-- This function works with the following output line consumers:
--
-- * 'sinkString'
-- * 'sinkText'
-- * 'sinkLazyText'
-- * 'sinkByteString'
-- * 'sinkLazyByteString'
-- * 'sinkHandle'
-- * 'sinkFile'
--
-- @since 0.0.1.0
runResource
  :: MonadUnliftIO m
  => SourceFormat
  -> Renderer.Options
  -> C.ConduitT () T.Text (C.ResourceT m) ()     -- ^ input line producer
  -> C.ConduitT T.Text C.Void (C.ResourceT m) r  -- ^ output line consumer
  -> m r
runResource sourceFormat rendererOpts source sink = C.runConduitRes $
    source .| transform sourceFormat rendererOpts .| sink

------------------------------------------------------------------------------
-- $Producers

-- | Produce input lines from a 'String' source
--
-- @since 0.0.1.0
sourceString
  :: Monad m
  => String
  -> C.ConduitT i T.Text m ()
sourceString = CC.yieldMany . map T.pack . lines

-- | Produce input lines from a 'T.Text' source
--
-- @since 0.0.1.0
sourceText
  :: Monad m
  => T.Text
  -> C.ConduitT i T.Text m ()
sourceText source = C.yield source .| CC.linesUnbounded

-- | Produce input lines from a lazy 'TL.Text' source
--
-- @since 0.0.1.0
sourceLazyText
  :: Monad m
  => TL.Text
  -> C.ConduitT i T.Text m ()
sourceLazyText source = CC.sourceLazy source .| CC.linesUnbounded

-- | Produce input lines from a 'BS.ByteString' source
--
-- @since 0.0.1.0
sourceByteString
  :: Monad m
  => BS.ByteString
  -> C.ConduitT i T.Text m ()
sourceByteString source =
    C.yield source .| CC.linesUnboundedAscii .| decodeUtf8LinesLenient

-- | Produce input lines from a lazy 'BS.ByteString' source
--
-- @since 0.0.1.0
sourceLazyByteString
  :: Monad m
  => BSL.ByteString
  -> C.ConduitT i T.Text m ()
sourceLazyByteString source =
    CC.sourceLazy source .| CC.linesUnboundedAscii .| decodeUtf8LinesLenient

-- | Produce input lines from a 'Handle' source
--
-- @since 0.0.1.0
sourceHandle
  :: MonadIO m
  => Handle
  -> C.ConduitT i T.Text m ()
sourceHandle source =
    CC.sourceHandle source .| CC.linesUnboundedAscii .| decodeUtf8LinesLenient

-- | Produce input lines from a file source
--
-- @since 0.0.1.0
sourceFile
  :: C.MonadResource m
  => FilePath
  -> C.ConduitT i T.Text m ()
sourceFile source =
    CC.sourceFile source .| CC.linesUnboundedAscii .| decodeUtf8LinesLenient

------------------------------------------------------------------------------
-- $Consumers

-- | Consume output lines, returning a 'String'
--
-- @since 0.0.1.0
sinkString
  :: Monad m
  => C.ConduitT T.Text o m String
sinkString = fmap TL.unpack $ CC.unlines .| CC.sinkLazy

-- | Consume output lines, returning 'T.Text'
--
-- @since 0.0.1.0
sinkText
  :: Monad m
  => C.ConduitT T.Text o m T.Text
sinkText = fmap TL.toStrict $ CC.unlines .| CC.sinkLazy

-- | Consume output lines, returning lazy 'T.Text'
--
-- @since 0.0.1.0
sinkLazyText
  :: Monad m
  => C.ConduitT T.Text o m TL.Text
sinkLazyText = CC.unlines .| CC.sinkLazy

-- | Consume output lines, returning a 'BS.ByteString'
--
-- @since 0.0.1.0
sinkByteString
  :: Monad m
  => C.ConduitT T.Text o m BS.ByteString
sinkByteString =
    fmap BSL.toStrict $ CC.unlines .| CC.encodeUtf8 .| CC.sinkLazy

-- | Consume output lines, returning a lazy 'BS.ByteString'
--
-- @since 0.0.1.0
sinkLazyByteString
  :: Monad m
  => C.ConduitT T.Text o m BSL.ByteString
sinkLazyByteString = CC.unlines .| CC.encodeUtf8 .| CC.sinkLazy

-- | Consume output lines, writing to a 'Handle'
--
-- @since 0.0.1.0
sinkHandle
  :: MonadIO m
  => Handle
  -> C.ConduitT T.Text o m ()
sinkHandle target = CC.encodeUtf8 .| CC.unlinesAscii .| CC.sinkHandle target

-- | Consume output lines, writing to a file
--
-- @since 0.0.1.0
sinkFile
  :: C.MonadResource m
  => FilePath
  -> C.ConduitT T.Text o m ()
sinkFile target = CC.encodeUtf8 .| CC.unlinesAscii .| CC.sinkFile target

------------------------------------------------------------------------------
-- $Transformers

-- | Transform input lines to output lines
--
-- @since 0.0.1.0
transform
  :: Monad m
  => SourceFormat
  -> Renderer.Options
  -> C.ConduitT T.Text T.Text m ()
transform sourceFormat rendererOpts
    = Parser.parse sourceFormat
    .| CL.concatMapAccum (\x n -> (n + 1, [(n, x)])) 1
    .| Renderer.render rendererOpts

------------------------------------------------------------------------------
-- $Internal

-- | Decode UTF-8 'BS.ByteString' lines to 'T.Text'
--
-- Note that 'CC.decodeUtf8Lenient' decodes UTF-8 streams and therefore
-- discards empty strings.  This version decodes UTF-8 lines and does not
-- discard empty lines.
decodeUtf8LinesLenient
  :: Monad m
  => C.ConduitT BS.ByteString T.Text m ()
decodeUtf8LinesLenient =
    C.awaitForever $ C.yield . TE.decodeUtf8With TEE.lenientDecode
