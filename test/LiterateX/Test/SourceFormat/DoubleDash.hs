{-# LANGUAGE OverloadedStrings #-}

module LiterateX.Test.SourceFormat.DoubleDash (tests) where

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

-- https://hackage.haskell.org/package/text
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy (Text)

-- (literatex)
import qualified LiterateX
import qualified LiterateX.Renderer as Renderer
import qualified LiterateX.Types.SourceFormat as SourceFormat
import qualified LiterateX.Types.TargetFormat as TargetFormat

------------------------------------------------------------------------------

rendererOpts :: Renderer.Options
rendererOpts = Renderer.Options
    { Renderer.targetFormat    = TargetFormat.PandocMarkdown
    , Renderer.codeLanguage    = Just "haskell"
    , Renderer.ignoreShebang   = True
    , Renderer.renderCode      = True
    , Renderer.numberCodeLines = True
    }

run' :: Text -> Renderer.Options -> Text
run' = flip $ LiterateX.transformTextToText SourceFormat.DoubleDash

run :: Text -> Text
run source = run' source rendererOpts

------------------------------------------------------------------------------

sourceStartShebangDoc :: Text
sourceStartShebangDoc = TL.unlines
    [ "#!/usr/bin/env stack"
    , ""
    , "-- This results in warnings!"
    , ""
    , "{- stack script --resolver lts-17.11 -}"
    , ""
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    ]

targetStartShebangDocIgnore :: Text
targetStartShebangDocIgnore = TL.unlines
    [ "This results in warnings!"
    , ""
    , "``` {.haskell .numberSource startFrom=\"5\"}"
    , "{- stack script --resolver lts-17.11 -}"
    , ""
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    ]

targetStartShebangDocNoIgnore :: Text
targetStartShebangDocNoIgnore = TL.unlines
    [ "``` {.numberSource startFrom=\"1\"}"
    , "#!/usr/bin/env stack"
    , "```"
    , ""
    , "This results in warnings!"
    , ""
    , "``` {.haskell .numberSource startFrom=\"5\"}"
    , "{- stack script --resolver lts-17.11 -}"
    , ""
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    ]

targetStartShebangDocNoCode :: Text
targetStartShebangDocNoCode = TL.unlines
    [ "This results in warnings!"
    ]

testStartShebangDoc :: TestTree
testStartShebangDoc = testGroup "startShebangDoc"
    [ testCase "Ignore" $
        targetStartShebangDocIgnore @=? run sourceStartShebangDoc
    , testCase "NoIgnore" $
        targetStartShebangDocNoIgnore @=?
          run' sourceStartShebangDoc rendererOpts
            { Renderer.ignoreShebang = False
            }
    , testCase "NoCode" $
        targetStartShebangDocNoCode @=?
          run' sourceStartShebangDoc rendererOpts
            { Renderer.ignoreShebang = False
            , Renderer.renderCode    = False
            }
    ]

------------------------------------------------------------------------------

sourceStartShebangCode :: Text
sourceStartShebangCode = TL.unlines
    [ "#!/usr/bin/env stack"
    , "{- stack script --resolver lts-17.11 -}"
    , ""
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , ""
    , "-- Test"
    ]

targetStartShebangCodeIgnore :: Text
targetStartShebangCodeIgnore = TL.unlines
    [ "``` {.haskell .numberSource startFrom=\"2\"}"
    , "{- stack script --resolver lts-17.11 -}"
    , ""
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    , ""
    , "Test"
    ]

targetStartShebangCodeNoIgnore :: Text
targetStartShebangCodeNoIgnore = TL.unlines
    [ "``` {.numberSource startFrom=\"1\"}"
    , "#!/usr/bin/env stack"
    , "```"
    , ""
    , "``` {.haskell .numberSource startFrom=\"2\"}"
    , "{- stack script --resolver lts-17.11 -}"
    , ""
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    , ""
    , "Test"
    ]

targetStartShebangCodeNoCode :: Text
targetStartShebangCodeNoCode = TL.unlines
    [ "Test"
    ]

testStartShebangCode :: TestTree
testStartShebangCode = testGroup "startShebangCode"
    [ testCase "Ignore" $
        targetStartShebangCodeIgnore @=? run sourceStartShebangCode
    , testCase "NoIgnore" $
        targetStartShebangCodeNoIgnore @=?
          run' sourceStartShebangCode rendererOpts
            { Renderer.ignoreShebang = False
            }
    , testCase "NoCode" $
        targetStartShebangCodeNoCode @=?
          run' sourceStartShebangCode rendererOpts
            { Renderer.ignoreShebang = False
            , Renderer.renderCode    = False
            }
    ]

------------------------------------------------------------------------------

sourceStartDoc :: Text
sourceStartDoc = TL.unlines
    [ "-- Test"
    , ""
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    ]

targetStartDoc :: Text
targetStartDoc = TL.unlines
    [ "Test"
    , ""
    , "``` {.haskell .numberSource startFrom=\"3\"}"
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    ]

testStartDoc :: TestTree
testStartDoc = testCase "startDoc" $
    targetStartDoc @=? run sourceStartDoc

------------------------------------------------------------------------------

sourceStartCode :: Text
sourceStartCode = TL.unlines
    [ "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , ""
    , "-- Test"
    ]

targetStartCode :: Text
targetStartCode = TL.unlines
    [ "``` {.haskell .numberSource startFrom=\"1\"}"
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    , ""
    , "Test"
    ]

testStartCode :: TestTree
testStartCode = testCase "startCode" $
    targetStartCode @=? run sourceStartCode

------------------------------------------------------------------------------

sourceStartDocBlank :: Text
sourceStartDocBlank = TL.unlines
    [ "--"
    , "-- Test"
    , ""
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    ]

targetStartDocBlank :: Text
targetStartDocBlank = TL.unlines
    [ "Test"
    , ""
    , "``` {.haskell .numberSource startFrom=\"4\"}"
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    ]

testStartDocBlank :: TestTree
testStartDocBlank = testCase "startDocBlank" $
    targetStartDocBlank @=? run sourceStartDocBlank

------------------------------------------------------------------------------

sourceStartCodeBlank :: Text
sourceStartCodeBlank = TL.unlines
    [ ""
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , ""
    , "-- Test"
    ]

targetStartCodeBlank :: Text
targetStartCodeBlank = TL.unlines
    [ "``` {.haskell .numberSource startFrom=\"2\"}"
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    , ""
    , "Test"
    ]

testStartCodeBlank :: TestTree
testStartCodeBlank = testCase "startCodeBlank" $
    targetStartCodeBlank @=? run sourceStartCodeBlank

------------------------------------------------------------------------------

sourceStartRuleDoc :: Text
sourceStartRuleDoc = TL.unlines
    [ "----------------------------------------------------------------------"
    , "-- Test"
    , ""
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    ]

targetStartRuleDoc :: Text
targetStartRuleDoc = TL.unlines
    [ "Test"
    , ""
    , "``` {.haskell .numberSource startFrom=\"4\"}"
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    ]

testStartRuleDoc :: TestTree
testStartRuleDoc = testCase "startRuleDoc" $
    targetStartRuleDoc @=? run sourceStartRuleDoc

------------------------------------------------------------------------------

sourceStartRuleCode :: Text
sourceStartRuleCode = TL.unlines
    [ "----------------------------------------------------------------------"
    , ""
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , ""
    , "-- Test"
    ]

targetStartRuleCode :: Text
targetStartRuleCode = TL.unlines
    [ "``` {.haskell .numberSource startFrom=\"3\"}"
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    , ""
    , "Test"
    ]

testStartRuleCode :: TestTree
testStartRuleCode = testCase "startRuleCode" $
    targetStartRuleCode @=? run sourceStartRuleCode

------------------------------------------------------------------------------

sourceCodeDoc :: Text
sourceCodeDoc = TL.unlines
    [ "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "-- Test"
    ]

targetCodeDoc :: Text
targetCodeDoc = TL.unlines
    [ "``` {.haskell .numberSource startFrom=\"1\"}"
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    , ""
    , "Test"
    ]

testCodeDoc :: TestTree
testCodeDoc = testCase "codeDoc" $
    targetCodeDoc @=? run sourceCodeDoc

------------------------------------------------------------------------------

sourceCodeCodeBlanksCode :: Text
sourceCodeCodeBlanksCode = TL.unlines
    [ "module Main (main) where"
    , ""
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    ]

targetCodeCodeBlanksCode :: Text
targetCodeCodeBlanksCode = TL.unlines
    [ "``` {.haskell .numberSource startFrom=\"1\"}"
    , "module Main (main) where"
    , ""
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    ]

testCodeCodeBlanksCode :: TestTree
testCodeCodeBlanksCode = testCase "codeCodeBlanksCode" $
    targetCodeCodeBlanksCode @=? run sourceCodeCodeBlanksCode

------------------------------------------------------------------------------

sourceCodeCodeBlanksDoc :: Text
sourceCodeCodeBlanksDoc = TL.unlines
    [ "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , ""
    , ""
    , "-- Test"
    ]

targetCodeCodeBlanksDoc :: Text
targetCodeCodeBlanksDoc = TL.unlines
    [ "``` {.haskell .numberSource startFrom=\"1\"}"
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    , ""
    , "Test"
    ]

testCodeCodeBlanksDoc :: TestTree
testCodeCodeBlanksDoc = testCase "codeCodeBlanksDoc" $
    targetCodeCodeBlanksDoc @=? run sourceCodeCodeBlanksDoc

------------------------------------------------------------------------------

sourceCodeDocBlanksDoc :: Text
sourceCodeDocBlanksDoc = TL.unlines
    [ "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "--"
    , "--"
    , "-- Test"
    ]

targetCodeDocBlanksDoc :: Text
targetCodeDocBlanksDoc = TL.unlines
    [ "``` {.haskell .numberSource startFrom=\"1\"}"
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    , ""
    , "Test"
    ]

testCodeDocBlanksDoc :: TestTree
testCodeDocBlanksDoc = testCase "codeDocBlanksDoc" $
    targetCodeDocBlanksDoc @=? run sourceCodeDocBlanksDoc

------------------------------------------------------------------------------

sourceCodeDocBlanksCode :: Text
sourceCodeDocBlanksCode = TL.unlines
    [ "module Main (main) where"
    , "--"
    , "--"
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    ]

targetCodeDocBlanksCode :: Text
targetCodeDocBlanksCode = TL.unlines
    [ "``` {.haskell .numberSource startFrom=\"1\"}"
    , "module Main (main) where"
    , "```"
    , ""
    , "``` {.haskell .numberSource startFrom=\"4\"}"
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    ]

testCodeDocBlanksCode :: TestTree
testCodeDocBlanksCode = testCase "codeDocBlanksCode" $
    targetCodeDocBlanksCode @=? run sourceCodeDocBlanksCode

------------------------------------------------------------------------------

sourceCodeRuleCode :: Text
sourceCodeRuleCode = TL.unlines
    [ "module Main (main) where"
    , "----------------------------------------------------------------------"
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    ]

targetCodeRuleCode :: Text
targetCodeRuleCode = TL.unlines
    [ "``` {.haskell .numberSource startFrom=\"1\"}"
    , "module Main (main) where"
    , "```"
    , ""
    , "``` {.haskell .numberSource startFrom=\"3\"}"
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    ]

testCodeRuleCode :: TestTree
testCodeRuleCode = testCase "codeRuleCode" $
    targetCodeRuleCode @=? run sourceCodeRuleCode

------------------------------------------------------------------------------

sourceCodeRuleDoc :: Text
sourceCodeRuleDoc = TL.unlines
    [ "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "----------------------------------------------------------------------"
    , "-- Test"
    ]

targetCodeRuleDoc :: Text
targetCodeRuleDoc = TL.unlines
    [ "``` {.haskell .numberSource startFrom=\"1\"}"
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    , ""
    , "Test"
    ]

testCodeRuleDoc :: TestTree
testCodeRuleDoc = testCase "codeRuleDoc" $
    targetCodeRuleDoc @=? run sourceCodeRuleDoc

------------------------------------------------------------------------------

sourceDocCode :: Text
sourceDocCode = TL.unlines
    [ "-- Test"
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    ]

targetDocCode :: Text
targetDocCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.haskell .numberSource startFrom=\"2\"}"
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    ]

testDocCode :: TestTree
testDocCode = testCase "docCode" $
    targetDocCode @=? run sourceDocCode

------------------------------------------------------------------------------

sourceDocDocBlanksDoc :: Text
sourceDocDocBlanksDoc = TL.unlines
    [ "-- one"
    , "--"
    , "--"
    , "-- two"
    ]

targetDocDocBlanksDoc :: Text
targetDocDocBlanksDoc = TL.unlines
    [ "one"
    , ""
    , "two"
    ]

testDocDocBlanksDoc :: TestTree
testDocDocBlanksDoc = testCase "docDocBlanksDoc" $
    targetDocDocBlanksDoc @=? run sourceDocDocBlanksDoc

------------------------------------------------------------------------------

sourceDocDocBlanksCode :: Text
sourceDocDocBlanksCode = TL.unlines
    [ "-- Test"
    , "--"
    , "--"
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    ]

targetDocDocBlanksCode :: Text
targetDocDocBlanksCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.haskell .numberSource startFrom=\"4\"}"
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    ]

testDocDocBlanksCode :: TestTree
testDocDocBlanksCode = testCase "docDocBlanksCode" $
    targetDocDocBlanksCode @=? run sourceDocDocBlanksCode

------------------------------------------------------------------------------

sourceDocCodeBlanksCode :: Text
sourceDocCodeBlanksCode = TL.unlines
    [ "-- Test"
    , ""
    , ""
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    ]

targetDocCodeBlanksCode :: Text
targetDocCodeBlanksCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.haskell .numberSource startFrom=\"4\"}"
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    ]

testDocCodeBlanksCode :: TestTree
testDocCodeBlanksCode = testCase "docCodeBlanksCode" $
    targetDocCodeBlanksCode @=? run sourceDocCodeBlanksCode

------------------------------------------------------------------------------

sourceDocCodeBlanksDoc :: Text
sourceDocCodeBlanksDoc = TL.unlines
    [ "-- one"
    , ""
    , ""
    , "-- two"
    ]

targetDocCodeBlanksDoc :: Text
targetDocCodeBlanksDoc = TL.unlines
    [ "one"
    , ""
    , "two"
    ]

testDocCodeBlanksDoc :: TestTree
testDocCodeBlanksDoc = testCase "docCodeBlanksDoc" $
    targetDocCodeBlanksDoc @=? run sourceDocCodeBlanksDoc

------------------------------------------------------------------------------

sourceDocRuleDoc :: Text
sourceDocRuleDoc = TL.unlines
    [ "-- one"
    , "----------------------------------------------------------------------"
    , "-- two"
    ]

targetDocRuleDoc :: Text
targetDocRuleDoc = TL.unlines
    [ "one"
    , ""
    , "two"
    ]

testDocRuleDoc :: TestTree
testDocRuleDoc = testCase "docRuleDoc" $
    targetDocRuleDoc @=? run sourceDocRuleDoc

------------------------------------------------------------------------------

sourceDocRuleCode :: Text
sourceDocRuleCode = TL.unlines
    [ "-- Test"
    , "----------------------------------------------------------------------"
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    ]

targetDocRuleCode :: Text
targetDocRuleCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.haskell .numberSource startFrom=\"3\"}"
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    ]

testDocRuleCode :: TestTree
testDocRuleCode = testCase "docRuleCode" $
    targetDocRuleCode @=? run sourceDocRuleCode

------------------------------------------------------------------------------

sourceEndDoc :: Text
sourceEndDoc = TL.unlines
    [ "-- Test"
    ]

targetEndDoc :: Text
targetEndDoc = TL.unlines
    [ "Test"
    ]

testEndDoc :: TestTree
testEndDoc = testCase "endDoc" $
    targetEndDoc @=? run sourceEndDoc

------------------------------------------------------------------------------

sourceEndDocBlanks :: Text
sourceEndDocBlanks = TL.unlines
    [ "--"
    , "--"
    ]

targetEndDocBlanks :: Text
targetEndDocBlanks = ""

testEndDocBlanks :: TestTree
testEndDocBlanks = testCase "endDocBlanks" $
    targetEndDocBlanks @=? run sourceEndDocBlanks

------------------------------------------------------------------------------

sourceEndCode :: Text
sourceEndCode = TL.unlines
    [ "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    ]

targetEndCode :: Text
targetEndCode = TL.unlines
    [ "``` {.haskell .numberSource startFrom=\"1\"}"
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    ]

testEndCode :: TestTree
testEndCode = testCase "endCode" $
    targetEndCode @=? run sourceEndCode

------------------------------------------------------------------------------

sourceEndCodeBlanks :: Text
sourceEndCodeBlanks = TL.unlines
    [ "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , ""
    , ""
    ]

targetEndCodeBlanks :: Text
targetEndCodeBlanks = TL.unlines
    [ "``` {.haskell .numberSource startFrom=\"1\"}"
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    , "```"
    ]

testEndCodeBlanks :: TestTree
testEndCodeBlanks = testCase "endCodeBlanks" $
    targetEndCodeBlanks @=? run sourceEndCodeBlanks

------------------------------------------------------------------------------

sourceEmpty :: Text
sourceEmpty = ""

sourceOnlyRule :: Text
sourceOnlyRule = "-----------------------------------------------------------"

sourceOnlyDocBlanks :: Text
sourceOnlyDocBlanks = TL.unlines
    [ "--"
    , "--"
    ]

sourceOnlyCodeBlanks :: Text
sourceOnlyCodeBlanks = TL.unlines
    [ ""
    , ""
    ]

targetEmpty :: Text
targetEmpty = ""

testEmpty :: TestTree
testEmpty = testCase "empty" $
    targetEmpty @=? run sourceEmpty

testOnlyRule :: TestTree
testOnlyRule = testCase "onlyRule" $
    targetEmpty @=? run sourceOnlyRule

testOnlyDocBlanks :: TestTree
testOnlyDocBlanks = testCase "onlyDocBlanks" $
    targetEmpty @=? run sourceOnlyDocBlanks

testOnlyCodeBlanks :: TestTree
testOnlyCodeBlanks = testCase "onlyCodeBlanks" $
    targetEmpty @=? run sourceOnlyCodeBlanks

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "DoubleDash"
    [ testStartShebangDoc
    , testStartShebangCode
    , testStartDoc
    , testStartCode
    , testStartDocBlank
    , testStartCodeBlank
    , testStartRuleDoc
    , testStartRuleCode
    , testCodeDoc
    , testCodeCodeBlanksCode
    , testCodeCodeBlanksDoc
    , testCodeDocBlanksDoc
    , testCodeDocBlanksCode
    , testCodeRuleCode
    , testCodeRuleDoc
    , testDocCode
    , testDocDocBlanksDoc
    , testDocDocBlanksCode
    , testDocCodeBlanksCode
    , testDocCodeBlanksDoc
    , testDocRuleDoc
    , testDocRuleCode
    , testEndDoc
    , testEndDocBlanks
    , testEndCode
    , testEndCodeBlanks
    , testEmpty
    , testOnlyRule
    , testOnlyDocBlanks
    , testOnlyCodeBlanks
    ]
