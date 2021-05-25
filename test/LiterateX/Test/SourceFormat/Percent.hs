{-# LANGUAGE OverloadedStrings #-}

module LiterateX.Test.SourceFormat.Percent (tests) where

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
    , Renderer.codeLanguage    = Just "erlang"
    , Renderer.ignoreShebang   = True
    , Renderer.renderCode      = True
    , Renderer.numberCodeLines = True
    }

run' :: Text -> Renderer.Options -> Text
run' = flip $ LiterateX.transformTextToText SourceFormat.Percent

run :: Text -> Text
run source = run' source rendererOpts

------------------------------------------------------------------------------

sourceStartShebangDoc :: Text
sourceStartShebangDoc = TL.unlines
    [ "#!/usr/bin/env escript"
    , ""
    , "% Test"
    , ""
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    ]

targetStartShebangDocIgnore :: Text
targetStartShebangDocIgnore = TL.unlines
    [ "Test"
    , ""
    , "``` {.erlang .numberSource startFrom=\"5\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , "```"
    ]

targetStartShebangDocNoIgnore :: Text
targetStartShebangDocNoIgnore = TL.unlines
    [ "``` {.numberSource startFrom=\"1\"}"
    , "#!/usr/bin/env escript"
    , "```"
    , ""
    , "Test"
    , ""
    , "``` {.erlang .numberSource startFrom=\"5\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , "```"
    ]

targetStartShebangDocNoCode :: Text
targetStartShebangDocNoCode = TL.unlines
    [ "Test"
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
    [ "#!/usr/bin/env escript"
    , ""
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , ""
    , "% Test"
    ]

targetStartShebangCodeIgnore :: Text
targetStartShebangCodeIgnore = TL.unlines
    [ "``` {.erlang .numberSource startFrom=\"3\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , "```"
    , ""
    , "Test"
    ]

targetStartShebangCodeNoIgnore :: Text
targetStartShebangCodeNoIgnore = TL.unlines
    [ "``` {.numberSource startFrom=\"1\"}"
    , "#!/usr/bin/env escript"
    , "```"
    , ""
    , "``` {.erlang .numberSource startFrom=\"3\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
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
    [ "% Test"
    , ""
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    ]

targetStartDoc :: Text
targetStartDoc = TL.unlines
    [ "Test"
    , ""
    , "``` {.erlang .numberSource startFrom=\"3\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , "```"
    ]

testStartDoc :: TestTree
testStartDoc = testCase "startDoc" $
    targetStartDoc @=? run sourceStartDoc

------------------------------------------------------------------------------

sourceStartCode :: Text
sourceStartCode = TL.unlines
    [ "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , ""
    , "% Test"
    ]

targetStartCode :: Text
targetStartCode = TL.unlines
    [ "``` {.erlang .numberSource startFrom=\"1\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
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
    [ "%"
    , "% Test"
    , ""
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    ]

targetStartDocBlank :: Text
targetStartDocBlank = TL.unlines
    [ "Test"
    , ""
    , "``` {.erlang .numberSource startFrom=\"4\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , "```"
    ]

testStartDocBlank :: TestTree
testStartDocBlank = testCase "startDocBlank" $
    targetStartDocBlank @=? run sourceStartDocBlank

------------------------------------------------------------------------------

sourceStartCodeBlank :: Text
sourceStartCodeBlank = TL.unlines
    [ ""
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , ""
    , "% Test"
    ]

targetStartCodeBlank :: Text
targetStartCodeBlank = TL.unlines
    [ "``` {.erlang .numberSource startFrom=\"2\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
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
    [ "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    , "% Test"
    , ""
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    ]

targetStartRuleDoc :: Text
targetStartRuleDoc = TL.unlines
    [ "Test"
    , ""
    , "``` {.erlang .numberSource startFrom=\"4\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , "```"
    ]

testStartRuleDoc :: TestTree
testStartRuleDoc = testCase "startRuleDoc" $
    targetStartRuleDoc @=? run sourceStartRuleDoc

------------------------------------------------------------------------------

sourceStartRuleCode :: Text
sourceStartRuleCode = TL.unlines
    [ "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    , ""
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , ""
    , "% Test"
    ]

targetStartRuleCode :: Text
targetStartRuleCode = TL.unlines
    [ "``` {.erlang .numberSource startFrom=\"3\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
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
    [ "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , "% Test"
    ]

targetCodeDoc :: Text
targetCodeDoc = TL.unlines
    [ "``` {.erlang .numberSource startFrom=\"1\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
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
    [ "-module(main)."
    , "-export([start/0])."
    , ""
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    ]

targetCodeCodeBlanksCode :: Text
targetCodeCodeBlanksCode = TL.unlines
    [ "``` {.erlang .numberSource startFrom=\"1\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , "```"
    ]

testCodeCodeBlanksCode :: TestTree
testCodeCodeBlanksCode = testCase "codeCodeBlanksCode" $
    targetCodeCodeBlanksCode @=? run sourceCodeCodeBlanksCode

------------------------------------------------------------------------------

sourceCodeCodeBlanksDoc :: Text
sourceCodeCodeBlanksDoc = TL.unlines
    [ "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , ""
    , ""
    , "% Test"
    ]

targetCodeCodeBlanksDoc :: Text
targetCodeCodeBlanksDoc = TL.unlines
    [ "``` {.erlang .numberSource startFrom=\"1\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
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
    [ "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , "%"
    , "%"
    , "% Test"
    ]

targetCodeDocBlanksDoc :: Text
targetCodeDocBlanksDoc = TL.unlines
    [ "``` {.erlang .numberSource startFrom=\"1\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
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
    [ "-module(main)."
    , "-export([start/0])."
    , "%"
    , "%"
    , "start() -> io:fwrite(\"Hello!\\n\")."
    ]

targetCodeDocBlanksCode :: Text
targetCodeDocBlanksCode = TL.unlines
    [ "``` {.erlang .numberSource startFrom=\"1\"}"
    , "-module(main)."
    , "-export([start/0])."
    , "```"
    , ""
    , "``` {.erlang .numberSource startFrom=\"5\"}"
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , "```"
    ]

testCodeDocBlanksCode :: TestTree
testCodeDocBlanksCode = testCase "codeDocBlanksCode" $
    targetCodeDocBlanksCode @=? run sourceCodeDocBlanksCode

------------------------------------------------------------------------------

sourceCodeRuleCode :: Text
sourceCodeRuleCode = TL.unlines
    [ "-module(main)."
    , "-export([start/0])."
    , "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    , "start() -> io:fwrite(\"Hello!\\n\")."
    ]

targetCodeRuleCode :: Text
targetCodeRuleCode = TL.unlines
    [ "``` {.erlang .numberSource startFrom=\"1\"}"
    , "-module(main)."
    , "-export([start/0])."
    , "```"
    , ""
    , "``` {.erlang .numberSource startFrom=\"4\"}"
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , "```"
    ]

testCodeRuleCode :: TestTree
testCodeRuleCode = testCase "codeRuleCode" $
    targetCodeRuleCode @=? run sourceCodeRuleCode

------------------------------------------------------------------------------

sourceCodeRuleDoc :: Text
sourceCodeRuleDoc = TL.unlines
    [ "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    , "% Test"
    ]

targetCodeRuleDoc :: Text
targetCodeRuleDoc = TL.unlines
    [ "``` {.erlang .numberSource startFrom=\"1\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
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
    [ "% Test"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    ]

targetDocCode :: Text
targetDocCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.erlang .numberSource startFrom=\"2\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , "```"
    ]

testDocCode :: TestTree
testDocCode = testCase "docCode" $
    targetDocCode @=? run sourceDocCode

------------------------------------------------------------------------------

sourceDocDocBlanksDoc :: Text
sourceDocDocBlanksDoc = TL.unlines
    [ "% one"
    , "%"
    , "%"
    , "% two"
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
    [ "% Test"
    , "%"
    , "%"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    ]

targetDocDocBlanksCode :: Text
targetDocDocBlanksCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.erlang .numberSource startFrom=\"4\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , "```"
    ]

testDocDocBlanksCode :: TestTree
testDocDocBlanksCode = testCase "docDocBlanksCode" $
    targetDocDocBlanksCode @=? run sourceDocDocBlanksCode

------------------------------------------------------------------------------

sourceDocCodeBlanksCode :: Text
sourceDocCodeBlanksCode = TL.unlines
    [ "% Test"
    , ""
    , ""
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    ]

targetDocCodeBlanksCode :: Text
targetDocCodeBlanksCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.erlang .numberSource startFrom=\"4\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , "```"
    ]

testDocCodeBlanksCode :: TestTree
testDocCodeBlanksCode = testCase "docCodeBlanksCode" $
    targetDocCodeBlanksCode @=? run sourceDocCodeBlanksCode

------------------------------------------------------------------------------

sourceDocCodeBlanksDoc :: Text
sourceDocCodeBlanksDoc = TL.unlines
    [ "% one"
    , ""
    , ""
    , "% two"
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
    [ "% one"
    , "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    , "% two"
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
    [ "% Test"
    , "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    ]

targetDocRuleCode :: Text
targetDocRuleCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.erlang .numberSource startFrom=\"3\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , "```"
    ]

testDocRuleCode :: TestTree
testDocRuleCode = testCase "docRuleCode" $
    targetDocRuleCode @=? run sourceDocRuleCode

------------------------------------------------------------------------------

sourceEndDoc :: Text
sourceEndDoc = TL.unlines
    [ "% Test"
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
    [ "%"
    , "%"
    ]

targetEndDocBlanks :: Text
targetEndDocBlanks = ""

testEndDocBlanks :: TestTree
testEndDocBlanks = testCase "endDocBlanks" $
    targetEndDocBlanks @=? run sourceEndDocBlanks

------------------------------------------------------------------------------

sourceEndCode :: Text
sourceEndCode = TL.unlines
    [ "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    ]

targetEndCode :: Text
targetEndCode = TL.unlines
    [ "``` {.erlang .numberSource startFrom=\"1\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , "```"
    ]

testEndCode :: TestTree
testEndCode = testCase "endCode" $
    targetEndCode @=? run sourceEndCode

------------------------------------------------------------------------------

sourceEndCodeBlanks :: Text
sourceEndCodeBlanks = TL.unlines
    [ "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , ""
    , ""
    ]

targetEndCodeBlanks :: Text
targetEndCodeBlanks = TL.unlines
    [ "``` {.erlang .numberSource startFrom=\"1\"}"
    , "-module(main)."
    , "-export([start/0])."
    , ""
    , "start() -> io:fwrite(\"Hello!\\n\")."
    , "```"
    ]

testEndCodeBlanks :: TestTree
testEndCodeBlanks = testCase "endCodeBlanks" $
    targetEndCodeBlanks @=? run sourceEndCodeBlanks

------------------------------------------------------------------------------

sourceEmpty :: Text
sourceEmpty = ""

sourceOnlyRule :: Text
sourceOnlyRule = "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"

sourceOnlyDocBlanks :: Text
sourceOnlyDocBlanks = TL.unlines
    [ "%"
    , "%"
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
tests = testGroup "Percent"
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
