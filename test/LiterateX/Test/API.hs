{-# LANGUAGE OverloadedStrings #-}

module LiterateX.Test.API (tests) where

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- https://hackage.haskell.org/package/filepath
import System.FilePath ((</>))

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

-- https://hackage.haskell.org/package/unliftio
import qualified UnliftIO.IO as IO
import UnliftIO.Temporary (withSystemTempDirectory)

-- (literatex)
import qualified LiterateX
import qualified LiterateX.Renderer as Renderer
import qualified LiterateX.Types.SourceFormat as SourceFormat
import qualified LiterateX.Types.TargetFormat as TargetFormat

------------------------------------------------------------------------------

sourceS :: String
sourceS = unlines
    [ "#!/usr/bin/env python"
    , ""
    , "# # Example Python Program"
    , "#"
    , "# This is an example program."
    , ""
    , ""
    , "######################################################################"
    , ""
    , "# ## Function `greet`"
    , "#"
    , "# This function prints a greeting."
    , "def greet(name: str) -> None:"
    , "    if name == '':"
    , "        print('Hello!')"
    , "        return"
    , ""
    , "    print(f'Hello {name}!')"
    , ""
    , ""
    , "if __name__ == '__main__':"
    , "    greet()"
    ]

sourceT :: T.Text
sourceT = TTC.fromS sourceS

sourceTL :: TL.Text
sourceTL = TTC.fromS sourceS

sourceBS :: BS.ByteString
sourceBS = TTC.fromS sourceS

sourceBSL :: BSL.ByteString
sourceBSL = TTC.fromS sourceS

------------------------------------------------------------------------------

targetS :: String
targetS = unlines
    [ "# Example Python Program"
    , ""
    , "This is an example program."
    , ""
    , "## Function `greet`"
    , ""
    , "This function prints a greeting."
    , ""
    , "``` {.python .numberSource startFrom=\"13\"}"
    , "def greet(name: str) -> None:"
    , "    if name == '':"
    , "        print('Hello!')"
    , "        return"
    , ""
    , "    print(f'Hello {name}!')"
    , ""
    , ""
    , "if __name__ == '__main__':"
    , "    greet()"
    , "```"
    ]

targetT :: T.Text
targetT = TTC.fromS targetS

targetTL :: TL.Text
targetTL = TTC.fromS targetS

targetBS :: BS.ByteString
targetBS = TTC.fromS targetS

targetBSL :: BSL.ByteString
targetBSL = TTC.fromS targetS

------------------------------------------------------------------------------

withSourceHandle
  :: String  -- ^ content
  -> (IO.Handle -> IO a)
  -> IO a
withSourceHandle content action =
    withSystemTempDirectory "literatex-test-" $ \tmpDirPath -> do
      let sourcePath = tmpDirPath </> "source"
      writeFile sourcePath content
      IO.withFile sourcePath IO.ReadMode action

------------------------------------------------------------------------------

withTargetHandle
  :: (IO.Handle -> IO ())
  -> IO String
withTargetHandle action =
    withSystemTempDirectory "literatex-test-" $ \tmpDirPath -> do
      let targetPath = tmpDirPath </> "target"
      IO.withFile targetPath IO.WriteMode action
      readFile targetPath

------------------------------------------------------------------------------

withSourceFile
  :: String  -- ^ content
  -> (FilePath -> IO a)
  -> IO a
withSourceFile content action =
    withSystemTempDirectory "literatex-test-" $ \tmpDirPath -> do
      let sourcePath = tmpDirPath </> "source"
      writeFile sourcePath content
      action sourcePath

------------------------------------------------------------------------------

withTargetFile
  :: (FilePath -> IO ())
  -> IO String
withTargetFile action =
    withSystemTempDirectory "literatex-test-" $ \tmpDirPath -> do
      let targetPath = tmpDirPath </> "target"
      action targetPath
      readFile targetPath

------------------------------------------------------------------------------

rendererOpts :: Renderer.Options
rendererOpts = Renderer.Options
    { Renderer.targetFormat    = TargetFormat.PandocMarkdown
    , Renderer.codeLanguage    = Just "python"
    , Renderer.ignoreShebang   = True
    , Renderer.renderCode      = True
    , Renderer.numberCodeLines = True
    }

------------------------------------------------------------------------------

testTransformTextToText :: TestTree
testTransformTextToText = testCase "transformTextToText" $
    targetTL @=?
      LiterateX.transformTextToText SourceFormat.Hash rendererOpts sourceTL

------------------------------------------------------------------------------

testTransformTextToHandle :: TestTree
testTransformTextToHandle = testCase "transformTextToHandle" $ do
    result <- withTargetHandle $
      LiterateX.transformTextToHandle SourceFormat.Hash rendererOpts sourceTL
    targetS @=? result

------------------------------------------------------------------------------

testTransformTextToFile :: TestTree
testTransformTextToFile = testCase "transformTextToFile" $ do
    result <- withTargetFile $
      LiterateX.transformTextToFile SourceFormat.Hash rendererOpts sourceTL
    targetS @=? result

------------------------------------------------------------------------------

testTransformHandleToText :: TestTree
testTransformHandleToText = testCase "transformHandleToText" $ do
    result <- withSourceHandle sourceS $
      LiterateX.transformHandleToText SourceFormat.Hash rendererOpts
    targetTL @=? result

------------------------------------------------------------------------------

testTransformHandleToHandle :: TestTree
testTransformHandleToHandle = testCase "transformHandleToHandle" $ do
    result <- withSourceHandle sourceS $ withTargetHandle .
      LiterateX.transformHandleToHandle SourceFormat.Hash rendererOpts
    targetS @=? result

------------------------------------------------------------------------------

testTransformHandleToFile :: TestTree
testTransformHandleToFile = testCase "transformHandleToFile" $ do
    result <- withSourceHandle sourceS $ withTargetFile .
      LiterateX.transformHandleToFile SourceFormat.Hash rendererOpts
    targetS @=? result

------------------------------------------------------------------------------

testTransformFileToText :: TestTree
testTransformFileToText = testCase "transformFileToText" $ do
    result <- withSourceFile sourceS $
      LiterateX.transformFileToText SourceFormat.Hash rendererOpts
    targetTL @=? result

------------------------------------------------------------------------------

testTransformFileToHandle :: TestTree
testTransformFileToHandle = testCase "transformFileToHandle" $ do
    result <- withSourceFile sourceS $ withTargetHandle .
      LiterateX.transformFileToHandle SourceFormat.Hash rendererOpts
    targetS @=? result

------------------------------------------------------------------------------

testTransformFileToFile :: TestTree
testTransformFileToFile = testCase "transformFileToFile" $ do
    result <- withSourceFile sourceS $ withTargetFile .
      LiterateX.transformFileToFile SourceFormat.Hash rendererOpts
    targetS @=? result

------------------------------------------------------------------------------

testRunPure :: TestTree
testRunPure = testGroup "runPure"
    [ testCase "sourceString" $
        targetTL @=? LiterateX.runPure SourceFormat.Hash rendererOpts
          (LiterateX.sourceString sourceS)
          LiterateX.sinkLazyText
    , testCase "sourceText" $
        targetTL @=? LiterateX.runPure SourceFormat.Hash rendererOpts
          (LiterateX.sourceText sourceT)
          LiterateX.sinkLazyText
    , testCase "sourceLazyText" $
        targetTL @=? LiterateX.runPure SourceFormat.Hash rendererOpts
          (LiterateX.sourceLazyText sourceTL)
          LiterateX.sinkLazyText
    , testCase "sourceByteString" $
        targetTL @=? LiterateX.runPure SourceFormat.Hash rendererOpts
          (LiterateX.sourceByteString sourceBS)
          LiterateX.sinkLazyText
    , testCase "sourceLazyByteString" $
        targetTL @=? LiterateX.runPure SourceFormat.Hash rendererOpts
          (LiterateX.sourceLazyByteString sourceBSL)
          LiterateX.sinkLazyText
    , testCase "sinkString" $
        targetS @=? LiterateX.runPure SourceFormat.Hash rendererOpts
          (LiterateX.sourceLazyText sourceTL)
          LiterateX.sinkString
    , testCase "sinkText" $
        targetT @=? LiterateX.runPure SourceFormat.Hash rendererOpts
          (LiterateX.sourceLazyText sourceTL)
          LiterateX.sinkText
    , testCase "sinkLazyText" $
        targetTL @=? LiterateX.runPure SourceFormat.Hash rendererOpts
          (LiterateX.sourceLazyText sourceTL)
          LiterateX.sinkLazyText
    , testCase "sinkByteString" $
        targetBS @=? LiterateX.runPure SourceFormat.Hash rendererOpts
          (LiterateX.sourceLazyText sourceTL)
          LiterateX.sinkByteString
    , testCase "sinkLazyByteString" $
        targetBSL @=? LiterateX.runPure SourceFormat.Hash rendererOpts
          (LiterateX.sourceLazyText sourceTL)
          LiterateX.sinkLazyByteString
    ]

------------------------------------------------------------------------------

testRunIO :: TestTree
testRunIO = testGroup "runIO"
    [ testCase "TextToHandle" $ do
        result <- withTargetHandle $ \target ->
          LiterateX.runIO SourceFormat.Hash rendererOpts
            (LiterateX.sourceLazyText sourceTL)
            (LiterateX.sinkHandle target)
        targetS @=? result
    , testCase "HandleToText" $ do
        result <- withSourceHandle sourceS $ \source ->
          LiterateX.runIO SourceFormat.Hash rendererOpts
            (LiterateX.sourceHandle source)
            LiterateX.sinkLazyText
        targetTL @=? result
    , testCase "HandleToHandle" $ do
        result <- withSourceHandle sourceS $ \source ->
          withTargetHandle $ \target ->
            LiterateX.runIO SourceFormat.Hash rendererOpts
              (LiterateX.sourceHandle source)
              (LiterateX.sinkHandle target)
        targetS @=? result
    ]

------------------------------------------------------------------------------

testRunResource :: TestTree
testRunResource = testGroup "runResource"
    [ testCase "TextToFile" $ do
        result <- withTargetFile $ \target ->
          LiterateX.runResource SourceFormat.Hash rendererOpts
            (LiterateX.sourceLazyText sourceTL)
            (LiterateX.sinkFile target)
        targetS @=? result
    , testCase "HandleToFile" $ do
        result <- withSourceHandle sourceS $ \source ->
          withTargetFile $ \target ->
            LiterateX.runResource SourceFormat.Hash rendererOpts
              (LiterateX.sourceHandle source)
              (LiterateX.sinkFile target)
        targetS @=? result
    , testCase "FileToText" $ do
        result <- withSourceFile sourceS $ \source ->
          LiterateX.runResource SourceFormat.Hash rendererOpts
            (LiterateX.sourceFile source)
            LiterateX.sinkLazyText
        targetTL @=? result
    , testCase "FileToHandle" $ do
        result <- withSourceFile sourceS $ \source ->
          withTargetHandle $ \target ->
            LiterateX.runResource SourceFormat.Hash rendererOpts
              (LiterateX.sourceFile source)
              (LiterateX.sinkHandle target)
        targetS @=? result
    , testCase "FileToFile" $ do
        result <- withSourceFile sourceS $ \source ->
          withTargetFile $ \target ->
            LiterateX.runResource SourceFormat.Hash rendererOpts
              (LiterateX.sourceFile source)
              (LiterateX.sinkFile target)
        targetS @=? result
    ]

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "LiterateX.Test.API"
    [ testTransformTextToText
    , testTransformTextToHandle
    , testTransformTextToFile
    , testTransformHandleToText
    , testTransformHandleToHandle
    , testTransformHandleToFile
    , testTransformFileToText
    , testTransformFileToHandle
    , testTransformFileToFile
    , testRunPure
    , testRunIO
    , testRunResource
    ]
