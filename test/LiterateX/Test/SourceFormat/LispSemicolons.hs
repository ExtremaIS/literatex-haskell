{-# LANGUAGE OverloadedStrings #-}

module LiterateX.Test.SourceFormat.LispSemicolons (tests) where

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
    , Renderer.codeLanguage    = Just "scheme"
    , Renderer.ignoreShebang   = True
    , Renderer.renderCode      = True
    , Renderer.numberCodeLines = True
    }

run' :: Text -> Renderer.Options -> Text
run' = flip $ LiterateX.transformTextToText SourceFormat.LispSemicolons

run :: Text -> Text
run source = run' source rendererOpts

------------------------------------------------------------------------------

sourceStartShebangDoc :: Text
sourceStartShebangDoc = TL.unlines
    [ "#!/usr/bin/env racket"
    , ""
    , "; Test"
    , ""
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    ]

targetStartShebangDocIgnore :: Text
targetStartShebangDocIgnore = TL.unlines
    [ "Test"
    , ""
    , "``` {.scheme .numberSource startFrom=\"5\"}"
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    , "```"
    ]

targetStartShebangDocNoIgnore :: Text
targetStartShebangDocNoIgnore = TL.unlines
    [ "``` {.numberSource startFrom=\"1\"}"
    , "#!/usr/bin/env racket"
    , "```"
    , ""
    , "Test"
    , ""
    , "``` {.scheme .numberSource startFrom=\"5\"}"
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
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
    [ "#!/usr/bin/env racket"
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    , ""
    , "; Test"
    ]

targetStartShebangCodeIgnore :: Text
targetStartShebangCodeIgnore = TL.unlines
    [ "``` {.scheme .numberSource startFrom=\"2\"}"
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    , "```"
    , ""
    , "Test"
    ]

targetStartShebangCodeNoIgnore :: Text
targetStartShebangCodeNoIgnore = TL.unlines
    [ "``` {.numberSource startFrom=\"1\"}"
    , "#!/usr/bin/env racket"
    , "```"
    , ""
    , "``` {.scheme .numberSource startFrom=\"2\"}"
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
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
    [ "; Test"
    , ""
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    ]

targetStartDoc :: Text
targetStartDoc = TL.unlines
    [ "Test"
    , ""
    , "``` {.scheme .numberSource startFrom=\"3\"}"
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    , "```"
    ]

testStartDoc :: TestTree
testStartDoc = testCase "startDoc" $
    targetStartDoc @=? run sourceStartDoc

------------------------------------------------------------------------------

sourceStartCode :: Text
sourceStartCode = TL.unlines
    [ "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    , ""
    , "; Test"
    ]

targetStartCode :: Text
targetStartCode = TL.unlines
    [ "``` {.scheme .numberSource startFrom=\"1\"}"
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
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
    [ ";"
    , "; Test"
    , ""
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    ]

targetStartDocBlank :: Text
targetStartDocBlank = TL.unlines
    [ "Test"
    , ""
    , "``` {.scheme .numberSource startFrom=\"4\"}"
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    , "```"
    ]

testStartDocBlank :: TestTree
testStartDocBlank = testCase "startDocBlank" $
    targetStartDocBlank @=? run sourceStartDocBlank

------------------------------------------------------------------------------

sourceStartCodeBlank :: Text
sourceStartCodeBlank = TL.unlines
    [ ""
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    , ""
    , "; Test"
    ]

targetStartCodeBlank :: Text
targetStartCodeBlank = TL.unlines
    [ "``` {.scheme .numberSource startFrom=\"2\"}"
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
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
    [ ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
    , "; Test"
    , ""
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    ]

targetStartRuleDoc :: Text
targetStartRuleDoc = TL.unlines
    [ "Test"
    , ""
    , "``` {.scheme .numberSource startFrom=\"4\"}"
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    , "```"
    ]

testStartRuleDoc :: TestTree
testStartRuleDoc = testCase "startRuleDoc" $
    targetStartRuleDoc @=? run sourceStartRuleDoc

------------------------------------------------------------------------------

sourceStartRuleCode :: Text
sourceStartRuleCode = TL.unlines
    [ ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
    , ""
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    , ""
    , "; Test"
    ]

targetStartRuleCode :: Text
targetStartRuleCode = TL.unlines
    [ "``` {.scheme .numberSource startFrom=\"3\"}"
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
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
    [ "#lang racket/base"
    , ""
    , "print('one')"
    , "; Test"
    ]

targetCodeDoc :: Text
targetCodeDoc = TL.unlines
    [ "``` {.scheme .numberSource startFrom=\"1\"}"
    , "#lang racket/base"
    , ""
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
    [ "#lang racket/base"
    , ""
    , ""
    , "print('one')"
    ]

targetCodeCodeBlanksCode :: Text
targetCodeCodeBlanksCode = TL.unlines
    [ "``` {.scheme .numberSource startFrom=\"1\"}"
    , "#lang racket/base"
    , ""
    , ""
    , "print('one')"
    , "```"
    ]

testCodeCodeBlanksCode :: TestTree
testCodeCodeBlanksCode = testCase "codeCodeBlanksCode" $
    targetCodeCodeBlanksCode @=? run sourceCodeCodeBlanksCode

------------------------------------------------------------------------------

sourceCodeCodeBlanksDoc :: Text
sourceCodeCodeBlanksDoc = TL.unlines
    [ "#lang racket/base"
    , ""
    , "print('one')"
    , ""
    , ""
    , "; Test"
    ]

targetCodeCodeBlanksDoc :: Text
targetCodeCodeBlanksDoc = TL.unlines
    [ "``` {.scheme .numberSource startFrom=\"1\"}"
    , "#lang racket/base"
    , ""
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
    [ "#lang racket/base"
    , ""
    , "print('one')"
    , ";"
    , ";"
    , "; Test"
    ]

targetCodeDocBlanksDoc :: Text
targetCodeDocBlanksDoc = TL.unlines
    [ "``` {.scheme .numberSource startFrom=\"1\"}"
    , "#lang racket/base"
    , ""
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
    [ "#lang racket/base"
    , ";"
    , ";"
    , "print('one')"
    ]

targetCodeDocBlanksCode :: Text
targetCodeDocBlanksCode = TL.unlines
    [ "``` {.scheme .numberSource startFrom=\"1\"}"
    , "#lang racket/base"
    , "```"
    , ""
    , "``` {.scheme .numberSource startFrom=\"4\"}"
    , "print('one')"
    , "```"
    ]

testCodeDocBlanksCode :: TestTree
testCodeDocBlanksCode = testCase "codeDocBlanksCode" $
    targetCodeDocBlanksCode @=? run sourceCodeDocBlanksCode

------------------------------------------------------------------------------

sourceCodeRuleCode :: Text
sourceCodeRuleCode = TL.unlines
    [ "#lang racket/base"
    , ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
    , "print('one')"
    ]

targetCodeRuleCode :: Text
targetCodeRuleCode = TL.unlines
    [ "``` {.scheme .numberSource startFrom=\"1\"}"
    , "#lang racket/base"
    , "```"
    , ""
    , "``` {.scheme .numberSource startFrom=\"3\"}"
    , "print('one')"
    , "```"
    ]

testCodeRuleCode :: TestTree
testCodeRuleCode = testCase "codeRuleCode" $
    targetCodeRuleCode @=? run sourceCodeRuleCode

------------------------------------------------------------------------------

sourceCodeRuleDoc :: Text
sourceCodeRuleDoc = TL.unlines
    [ "#lang racket/base"
    , ""
    , "print('one')"
    , ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
    , "; Test"
    ]

targetCodeRuleDoc :: Text
targetCodeRuleDoc = TL.unlines
    [ "``` {.scheme .numberSource startFrom=\"1\"}"
    , "#lang racket/base"
    , ""
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
    [ "; Test"
    , "#lang racket/base"
    , ""
    , "print('one')"
    ]

targetDocCode :: Text
targetDocCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.scheme .numberSource startFrom=\"2\"}"
    , "#lang racket/base"
    , ""
    , "print('one')"
    , "```"
    ]

testDocCode :: TestTree
testDocCode = testCase "docCode" $
    targetDocCode @=? run sourceDocCode

------------------------------------------------------------------------------

sourceDocDocBlanksDoc :: Text
sourceDocDocBlanksDoc = TL.unlines
    [ "; one"
    , ";"
    , ";"
    , "; two"
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
    [ "; Test"
    , ";"
    , ";"
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    ]

targetDocDocBlanksCode :: Text
targetDocDocBlanksCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.scheme .numberSource startFrom=\"4\"}"
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    , "```"
    ]

testDocDocBlanksCode :: TestTree
testDocDocBlanksCode = testCase "docDocBlanksCode" $
    targetDocDocBlanksCode @=? run sourceDocDocBlanksCode

------------------------------------------------------------------------------

sourceDocCodeBlanksCode :: Text
sourceDocCodeBlanksCode = TL.unlines
    [ "; Test"
    , ""
    , ""
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    ]

targetDocCodeBlanksCode :: Text
targetDocCodeBlanksCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.scheme .numberSource startFrom=\"4\"}"
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    , "```"
    ]

testDocCodeBlanksCode :: TestTree
testDocCodeBlanksCode = testCase "docCodeBlanksCode" $
    targetDocCodeBlanksCode @=? run sourceDocCodeBlanksCode

------------------------------------------------------------------------------

sourceDocCodeBlanksDoc :: Text
sourceDocCodeBlanksDoc = TL.unlines
    [ "; one"
    , ""
    , ""
    , "; two"
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
    [ "; one"
    , ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
    , "; two"
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
    [ "; Test"
    , ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    ]

targetDocRuleCode :: Text
targetDocRuleCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.scheme .numberSource startFrom=\"3\"}"
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    , "```"
    ]

testDocRuleCode :: TestTree
testDocRuleCode = testCase "docRuleCode" $
    targetDocRuleCode @=? run sourceDocRuleCode

------------------------------------------------------------------------------

sourceEndDoc :: Text
sourceEndDoc = TL.unlines
    [ "; Test"
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
    [ ";"
    , ";"
    ]

targetEndDocBlanks :: Text
targetEndDocBlanks = ""

testEndDocBlanks :: TestTree
testEndDocBlanks = testCase "endDocBlanks" $
    targetEndDocBlanks @=? run sourceEndDocBlanks

------------------------------------------------------------------------------

sourceEndCode :: Text
sourceEndCode = TL.unlines
    [ "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    ]

targetEndCode :: Text
targetEndCode = TL.unlines
    [ "``` {.scheme .numberSource startFrom=\"1\"}"
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    , "```"
    ]

testEndCode :: TestTree
testEndCode = testCase "endCode" $
    targetEndCode @=? run sourceEndCode

------------------------------------------------------------------------------

sourceEndCodeBlanks :: Text
sourceEndCodeBlanks = TL.unlines
    [ "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    , ""
    , ""
    ]

targetEndCodeBlanks :: Text
targetEndCodeBlanks = TL.unlines
    [ "``` {.scheme .numberSource startFrom=\"1\"}"
    , "#lang racket/base"
    , ""
    , "(println \"Hello!\")"
    , "```"
    ]

testEndCodeBlanks :: TestTree
testEndCodeBlanks = testCase "endCodeBlanks" $
    targetEndCodeBlanks @=? run sourceEndCodeBlanks

------------------------------------------------------------------------------

sourceEmpty :: Text
sourceEmpty = ""

sourceOnlyRule :: Text
sourceOnlyRule = ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"

sourceOnlyDocBlanks :: Text
sourceOnlyDocBlanks = TL.unlines
    [ ";"
    , ";"
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

testLispSemicolons :: TestTree
testLispSemicolons = testGroup "lispSemicolons"
    [ testCase "oneSemicolonDoc" $
        target @=? run (sourceSemicolonDoc 1)
    , testCase "twoSemicolonDoc" $
        target @=? run (sourceSemicolonDoc 2)
    , testCase "threeSemicolonDoc" $
        target @=? run (sourceSemicolonDoc 3)
    , testCase "fourSemicolonDoc" $
        target @=? run (sourceSemicolonDoc 4)
    ]
  where
    sourceSemicolonDoc :: Int -> Text
    sourceSemicolonDoc count =
      TL.concat [TL.pack (replicate count ';'), " ", target]

    target :: Text
    target = TL.unlines ["Test"]

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "LispSemicolons"
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
    , testLispSemicolons
    ]
