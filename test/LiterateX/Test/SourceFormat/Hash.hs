{-# LANGUAGE OverloadedStrings #-}

module LiterateX.Test.SourceFormat.Hash (tests) where

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
    , Renderer.codeLanguage    = Just "python"
    , Renderer.ignoreShebang   = True
    , Renderer.renderCode      = True
    , Renderer.numberCodeLines = True
    }

run' :: Text -> Renderer.Options -> Text
run' = flip $ LiterateX.transformTextToText SourceFormat.Hash

run :: Text -> Text
run source = run' source rendererOpts

------------------------------------------------------------------------------

sourceStartShebangDoc :: Text
sourceStartShebangDoc = TL.unlines
    [ "#!/usr/bin/env python"
    , ""
    , "# Test"
    , ""
    , "print('Hello!')"
    ]

targetStartShebangDocIgnore :: Text
targetStartShebangDocIgnore = TL.unlines
    [ "Test"
    , ""
    , "``` {.python .numberSource startFrom=\"5\"}"
    , "print('Hello!')"
    , "```"
    ]

targetStartShebangDocNoIgnore :: Text
targetStartShebangDocNoIgnore = TL.unlines
    [ "``` {.numberSource startFrom=\"1\"}"
    , "#!/usr/bin/env python"
    , "```"
    , ""
    , "Test"
    , ""
    , "``` {.python .numberSource startFrom=\"5\"}"
    , "print('Hello!')"
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
    [ "#!/usr/bin/env python"
    , ""
    , "print('Hello!')"
    , ""
    , "# Test"
    ]

targetStartShebangCodeIgnore :: Text
targetStartShebangCodeIgnore = TL.unlines
    [ "``` {.python .numberSource startFrom=\"3\"}"
    , "print('Hello!')"
    , "```"
    , ""
    , "Test"
    ]

targetStartShebangCodeNoIgnore :: Text
targetStartShebangCodeNoIgnore = TL.unlines
    [ "``` {.numberSource startFrom=\"1\"}"
    , "#!/usr/bin/env python"
    , "```"
    , ""
    , "``` {.python .numberSource startFrom=\"3\"}"
    , "print('Hello!')"
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
    [ "# Test"
    , ""
    , "print('Hello!')"
    ]

targetStartDoc :: Text
targetStartDoc = TL.unlines
    [ "Test"
    , ""
    , "``` {.python .numberSource startFrom=\"3\"}"
    , "print('Hello!')"
    , "```"
    ]

testStartDoc :: TestTree
testStartDoc = testCase "startDoc" $
    targetStartDoc @=? run sourceStartDoc

------------------------------------------------------------------------------

sourceStartCode :: Text
sourceStartCode = TL.unlines
    [ "print('Hello!')"
    , ""
    , "# Test"
    ]

targetStartCode :: Text
targetStartCode = TL.unlines
    [ "``` {.python .numberSource startFrom=\"1\"}"
    , "print('Hello!')"
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
    [ "#"
    , "# Test"
    , ""
    , "print('Hello!')"
    ]

targetStartDocBlank :: Text
targetStartDocBlank = TL.unlines
    [ "Test"
    , ""
    , "``` {.python .numberSource startFrom=\"4\"}"
    , "print('Hello!')"
    , "```"
    ]

testStartDocBlank :: TestTree
testStartDocBlank = testCase "startDocBlank" $
    targetStartDocBlank @=? run sourceStartDocBlank

------------------------------------------------------------------------------

sourceStartCodeBlank :: Text
sourceStartCodeBlank = TL.unlines
    [ ""
    , "print('Hello!')"
    , ""
    , "# Test"
    ]

targetStartCodeBlank :: Text
targetStartCodeBlank = TL.unlines
    [ "``` {.python .numberSource startFrom=\"2\"}"
    , "print('Hello!')"
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
    [ "######################################################################"
    , "# Test"
    , ""
    , "print('Hello!')"
    ]

targetStartRuleDoc :: Text
targetStartRuleDoc = TL.unlines
    [ "Test"
    , ""
    , "``` {.python .numberSource startFrom=\"4\"}"
    , "print('Hello!')"
    , "```"
    ]

testStartRuleDoc :: TestTree
testStartRuleDoc = testCase "startRuleDoc" $
    targetStartRuleDoc @=? run sourceStartRuleDoc

------------------------------------------------------------------------------

sourceStartRuleCode :: Text
sourceStartRuleCode = TL.unlines
    [ "######################################################################"
    , ""
    , "print('Hello!')"
    , ""
    , "# Test"
    ]

targetStartRuleCode :: Text
targetStartRuleCode = TL.unlines
    [ "``` {.python .numberSource startFrom=\"3\"}"
    , "print('Hello!')"
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
    [ "print('one')"
    , "# Test"
    ]

targetCodeDoc :: Text
targetCodeDoc = TL.unlines
    [ "``` {.python .numberSource startFrom=\"1\"}"
    , "print('one')"
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
    [ "print('one')"
    , ""
    , ""
    , "print('two')"
    ]

targetCodeCodeBlanksCode :: Text
targetCodeCodeBlanksCode = TL.unlines
    [ "``` {.python .numberSource startFrom=\"1\"}"
    , "print('one')"
    , ""
    , ""
    , "print('two')"
    , "```"
    ]

testCodeCodeBlanksCode :: TestTree
testCodeCodeBlanksCode = testCase "codeCodeBlanksCode" $
    targetCodeCodeBlanksCode @=? run sourceCodeCodeBlanksCode

------------------------------------------------------------------------------

sourceCodeCodeBlanksDoc :: Text
sourceCodeCodeBlanksDoc = TL.unlines
    [ "print('one')"
    , ""
    , ""
    , "# Test"
    ]

targetCodeCodeBlanksDoc :: Text
targetCodeCodeBlanksDoc = TL.unlines
    [ "``` {.python .numberSource startFrom=\"1\"}"
    , "print('one')"
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
    [ "print('one')"
    , "#"
    , "#"
    , "# Test"
    ]

targetCodeDocBlanksDoc :: Text
targetCodeDocBlanksDoc = TL.unlines
    [ "``` {.python .numberSource startFrom=\"1\"}"
    , "print('one')"
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
    [ "print('one')"
    , "#"
    , "#"
    , "print('two')"
    ]

targetCodeDocBlanksCode :: Text
targetCodeDocBlanksCode = TL.unlines
    [ "``` {.python .numberSource startFrom=\"1\"}"
    , "print('one')"
    , "```"
    , ""
    , "``` {.python .numberSource startFrom=\"4\"}"
    , "print('two')"
    , "```"
    ]

testCodeDocBlanksCode :: TestTree
testCodeDocBlanksCode = testCase "codeDocBlanksCode" $
    targetCodeDocBlanksCode @=? run sourceCodeDocBlanksCode

------------------------------------------------------------------------------

sourceCodeRuleCode :: Text
sourceCodeRuleCode = TL.unlines
    [ "print('one')"
    , "######################################################################"
    , "print('two')"
    ]

targetCodeRuleCode :: Text
targetCodeRuleCode = TL.unlines
    [ "``` {.python .numberSource startFrom=\"1\"}"
    , "print('one')"
    , "```"
    , ""
    , "``` {.python .numberSource startFrom=\"3\"}"
    , "print('two')"
    , "```"
    ]

testCodeRuleCode :: TestTree
testCodeRuleCode = testCase "codeRuleCode" $
    targetCodeRuleCode @=? run sourceCodeRuleCode

------------------------------------------------------------------------------

sourceCodeRuleDoc :: Text
sourceCodeRuleDoc = TL.unlines
    [ "print('one')"
    , "######################################################################"
    , "# Test"
    ]

targetCodeRuleDoc :: Text
targetCodeRuleDoc = TL.unlines
    [ "``` {.python .numberSource startFrom=\"1\"}"
    , "print('one')"
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
    [ "# Test"
    , "print('one')"
    ]

targetDocCode :: Text
targetDocCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.python .numberSource startFrom=\"2\"}"
    , "print('one')"
    , "```"
    ]

testDocCode :: TestTree
testDocCode = testCase "docCode" $
    targetDocCode @=? run sourceDocCode

------------------------------------------------------------------------------

sourceDocDocBlanksDoc :: Text
sourceDocDocBlanksDoc = TL.unlines
    [ "# one"
    , "#"
    , "#"
    , "# two"
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
    [ "# Test"
    , "#"
    , "#"
    , "print('Hello!')"
    ]

targetDocDocBlanksCode :: Text
targetDocDocBlanksCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.python .numberSource startFrom=\"4\"}"
    , "print('Hello!')"
    , "```"
    ]

testDocDocBlanksCode :: TestTree
testDocDocBlanksCode = testCase "docDocBlanksCode" $
    targetDocDocBlanksCode @=? run sourceDocDocBlanksCode

------------------------------------------------------------------------------

sourceDocCodeBlanksCode :: Text
sourceDocCodeBlanksCode = TL.unlines
    [ "# Test"
    , ""
    , ""
    , "print('Hello!')"
    ]

targetDocCodeBlanksCode :: Text
targetDocCodeBlanksCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.python .numberSource startFrom=\"4\"}"
    , "print('Hello!')"
    , "```"
    ]

testDocCodeBlanksCode :: TestTree
testDocCodeBlanksCode = testCase "docCodeBlanksCode" $
    targetDocCodeBlanksCode @=? run sourceDocCodeBlanksCode

------------------------------------------------------------------------------

sourceDocCodeBlanksDoc :: Text
sourceDocCodeBlanksDoc = TL.unlines
    [ "# one"
    , ""
    , ""
    , "# two"
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
    [ "# one"
    , "######################################################################"
    , "# two"
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
    [ "# Test"
    , "######################################################################"
    , "print('Hello!')"
    ]

targetDocRuleCode :: Text
targetDocRuleCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.python .numberSource startFrom=\"3\"}"
    , "print('Hello!')"
    , "```"
    ]

testDocRuleCode :: TestTree
testDocRuleCode = testCase "docRuleCode" $
    targetDocRuleCode @=? run sourceDocRuleCode

------------------------------------------------------------------------------

sourceEndDoc :: Text
sourceEndDoc = TL.unlines
    [ "# Test"
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
    [ "#"
    , "#"
    ]

targetEndDocBlanks :: Text
targetEndDocBlanks = ""

testEndDocBlanks :: TestTree
testEndDocBlanks = testCase "endDocBlanks" $
    targetEndDocBlanks @=? run sourceEndDocBlanks

------------------------------------------------------------------------------

sourceEndCode :: Text
sourceEndCode = TL.unlines
    [ "print('Hello!')"
    ]

targetEndCode :: Text
targetEndCode = TL.unlines
    [ "``` {.python .numberSource startFrom=\"1\"}"
    , "print('Hello!')"
    , "```"
    ]

testEndCode :: TestTree
testEndCode = testCase "endCode" $
    targetEndCode @=? run sourceEndCode

------------------------------------------------------------------------------

sourceEndCodeBlanks :: Text
sourceEndCodeBlanks = TL.unlines
    [ "print('Hello!')"
    , ""
    , ""
    ]

targetEndCodeBlanks :: Text
targetEndCodeBlanks = TL.unlines
    [ "``` {.python .numberSource startFrom=\"1\"}"
    , "print('Hello!')"
    , "```"
    ]

testEndCodeBlanks :: TestTree
testEndCodeBlanks = testCase "endCodeBlanks" $
    targetEndCodeBlanks @=? run sourceEndCodeBlanks

------------------------------------------------------------------------------

sourceEmpty :: Text
sourceEmpty = ""

sourceOnlyRule :: Text
sourceOnlyRule = "###########################################################"

sourceOnlyDocBlanks :: Text
sourceOnlyDocBlanks = TL.unlines
    [ "#"
    , "#"
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
tests = testGroup "Hash"
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
