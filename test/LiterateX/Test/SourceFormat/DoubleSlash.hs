{-# LANGUAGE OverloadedStrings #-}

module LiterateX.Test.SourceFormat.DoubleSlash (tests) where

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
    , Renderer.codeLanguage    = Just "rust"
    , Renderer.ignoreShebang   = True
    , Renderer.renderCode      = True
    , Renderer.numberCodeLines = True
    }

run' :: Text -> Renderer.Options -> Text
run' = flip $ LiterateX.transformTextToText SourceFormat.DoubleSlash

run :: Text -> Text
run source = run' source rendererOpts

------------------------------------------------------------------------------

sourceStartShebangDoc :: Text
sourceStartShebangDoc = TL.unlines
    [ "#!/usr/bin/env run-cargo-script"
    , ""
    , "// Test"
    , ""
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    ]

targetStartShebangDocIgnore :: Text
targetStartShebangDocIgnore = TL.unlines
    [ "Test"
    , ""
    , "``` {.rust .numberSource startFrom=\"5\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , "```"
    ]

targetStartShebangDocNoIgnore :: Text
targetStartShebangDocNoIgnore = TL.unlines
    [ "``` {.numberSource startFrom=\"1\"}"
    , "#!/usr/bin/env run-cargo-script"
    , "```"
    , ""
    , "Test"
    , ""
    , "``` {.rust .numberSource startFrom=\"5\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
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
    [ "#!/usr/bin/env run-cargo-script"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , ""
    , "// Test"
    ]

targetStartShebangCodeIgnore :: Text
targetStartShebangCodeIgnore = TL.unlines
    [ "``` {.rust .numberSource startFrom=\"2\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , "```"
    , ""
    , "Test"
    ]

targetStartShebangCodeNoIgnore :: Text
targetStartShebangCodeNoIgnore = TL.unlines
    [ "``` {.numberSource startFrom=\"1\"}"
    , "#!/usr/bin/env run-cargo-script"
    , "```"
    , ""
    , "``` {.rust .numberSource startFrom=\"2\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
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
    [ "// Test"
    , ""
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    ]

targetStartDoc :: Text
targetStartDoc = TL.unlines
    [ "Test"
    , ""
    , "``` {.rust .numberSource startFrom=\"3\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , "```"
    ]

testStartDoc :: TestTree
testStartDoc = testCase "startDoc" $
    targetStartDoc @=? run sourceStartDoc

------------------------------------------------------------------------------

sourceStartCode :: Text
sourceStartCode = TL.unlines
    [ "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , ""
    , "// Test"
    ]

targetStartCode :: Text
targetStartCode = TL.unlines
    [ "``` {.rust .numberSource startFrom=\"1\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
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
    [ "//"
    , "// Test"
    , ""
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    ]

targetStartDocBlank :: Text
targetStartDocBlank = TL.unlines
    [ "Test"
    , ""
    , "``` {.rust .numberSource startFrom=\"4\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , "```"
    ]

testStartDocBlank :: TestTree
testStartDocBlank = testCase "startDocBlank" $
    targetStartDocBlank @=? run sourceStartDocBlank

------------------------------------------------------------------------------

sourceStartCodeBlank :: Text
sourceStartCodeBlank = TL.unlines
    [ ""
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , ""
    , "// Test"
    ]

targetStartCodeBlank :: Text
targetStartCodeBlank = TL.unlines
    [ "``` {.rust .numberSource startFrom=\"2\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
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
    [ "//////////////////////////////////////////////////////////////////////"
    , "// Test"
    , ""
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    ]

targetStartRuleDoc :: Text
targetStartRuleDoc = TL.unlines
    [ "Test"
    , ""
    , "``` {.rust .numberSource startFrom=\"4\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , "```"
    ]

testStartRuleDoc :: TestTree
testStartRuleDoc = testCase "startRuleDoc" $
    targetStartRuleDoc @=? run sourceStartRuleDoc

------------------------------------------------------------------------------

sourceStartRuleCode :: Text
sourceStartRuleCode = TL.unlines
    [ "//////////////////////////////////////////////////////////////////////"
    , ""
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , ""
    , "// Test"
    ]

targetStartRuleCode :: Text
targetStartRuleCode = TL.unlines
    [ "``` {.rust .numberSource startFrom=\"3\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
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
    [ "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , "// Test"
    ]

targetCodeDoc :: Text
targetCodeDoc = TL.unlines
    [ "``` {.rust .numberSource startFrom=\"1\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
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
    [ "fn main() {"
    , "    println!(\"Hello...\");"
    , ""
    , ""
    , "    println!(\"world!\");"
    , "}"
    ]

targetCodeCodeBlanksCode :: Text
targetCodeCodeBlanksCode = TL.unlines
    [ "``` {.rust .numberSource startFrom=\"1\"}"
    , "fn main() {"
    , "    println!(\"Hello...\");"
    , ""
    , ""
    , "    println!(\"world!\");"
    , "}"
    , "```"
    ]

testCodeCodeBlanksCode :: TestTree
testCodeCodeBlanksCode = testCase "codeCodeBlanksCode" $
    targetCodeCodeBlanksCode @=? run sourceCodeCodeBlanksCode

------------------------------------------------------------------------------

sourceCodeCodeBlanksDoc :: Text
sourceCodeCodeBlanksDoc = TL.unlines
    [ "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , ""
    , ""
    , "// Test"
    ]

targetCodeCodeBlanksDoc :: Text
targetCodeCodeBlanksDoc = TL.unlines
    [ "``` {.rust .numberSource startFrom=\"1\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
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
    [ "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , "//"
    , "//"
    , "// Test"
    ]

targetCodeDocBlanksDoc :: Text
targetCodeDocBlanksDoc = TL.unlines
    [ "``` {.rust .numberSource startFrom=\"1\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
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
    [ "fn main() {"
    , "    println!(\"Hello...\");"
    , "//"
    , "//"
    , "    println!(\"world!\");"
    , "}"
    ]

targetCodeDocBlanksCode :: Text
targetCodeDocBlanksCode = TL.unlines
    [ "``` {.rust .numberSource startFrom=\"1\"}"
    , "fn main() {"
    , "    println!(\"Hello...\");"
    , "```"
    , ""
    , "``` {.rust .numberSource startFrom=\"5\"}"
    , "    println!(\"world!\");"
    , "}"
    , "```"
    ]

testCodeDocBlanksCode :: TestTree
testCodeDocBlanksCode = testCase "codeDocBlanksCode" $
    targetCodeDocBlanksCode @=? run sourceCodeDocBlanksCode

------------------------------------------------------------------------------

sourceCodeRuleCode :: Text
sourceCodeRuleCode = TL.unlines
    [ "fn main() {"
    , "    println!(\"Hello...\");"
    , "//////////////////////////////////////////////////////////////////////"
    , "    println!(\"world!\");"
    , "}"
    ]

targetCodeRuleCode :: Text
targetCodeRuleCode = TL.unlines
    [ "``` {.rust .numberSource startFrom=\"1\"}"
    , "fn main() {"
    , "    println!(\"Hello...\");"
    , "```"
    , ""
    , "``` {.rust .numberSource startFrom=\"4\"}"
    , "    println!(\"world!\");"
    , "}"
    , "```"
    ]

testCodeRuleCode :: TestTree
testCodeRuleCode = testCase "codeRuleCode" $
    targetCodeRuleCode @=? run sourceCodeRuleCode

------------------------------------------------------------------------------

sourceCodeRuleDoc :: Text
sourceCodeRuleDoc = TL.unlines
    [ "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , "//////////////////////////////////////////////////////////////////////"
    , "// Test"
    ]

targetCodeRuleDoc :: Text
targetCodeRuleDoc = TL.unlines
    [ "``` {.rust .numberSource startFrom=\"1\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
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
    [ "// Test"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    ]

targetDocCode :: Text
targetDocCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.rust .numberSource startFrom=\"2\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , "```"
    ]

testDocCode :: TestTree
testDocCode = testCase "docCode" $
    targetDocCode @=? run sourceDocCode

------------------------------------------------------------------------------

sourceDocDocBlanksDoc :: Text
sourceDocDocBlanksDoc = TL.unlines
    [ "// one"
    , "//"
    , "//"
    , "// two"
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
    [ "// Test"
    , "//"
    , "//"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    ]

targetDocDocBlanksCode :: Text
targetDocDocBlanksCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.rust .numberSource startFrom=\"4\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , "```"
    ]

testDocDocBlanksCode :: TestTree
testDocDocBlanksCode = testCase "docDocBlanksCode" $
    targetDocDocBlanksCode @=? run sourceDocDocBlanksCode

------------------------------------------------------------------------------

sourceDocCodeBlanksCode :: Text
sourceDocCodeBlanksCode = TL.unlines
    [ "// Test"
    , ""
    , ""
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    ]

targetDocCodeBlanksCode :: Text
targetDocCodeBlanksCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.rust .numberSource startFrom=\"4\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , "```"
    ]

testDocCodeBlanksCode :: TestTree
testDocCodeBlanksCode = testCase "docCodeBlanksCode" $
    targetDocCodeBlanksCode @=? run sourceDocCodeBlanksCode

------------------------------------------------------------------------------

sourceDocCodeBlanksDoc :: Text
sourceDocCodeBlanksDoc = TL.unlines
    [ "// one"
    , ""
    , ""
    , "// two"
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
    [ "// one"
    , "//////////////////////////////////////////////////////////////////////"
    , "// two"
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
    [ "// Test"
    , "//////////////////////////////////////////////////////////////////////"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    ]

targetDocRuleCode :: Text
targetDocRuleCode = TL.unlines
    [ "Test"
    , ""
    , "``` {.rust .numberSource startFrom=\"3\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , "```"
    ]

testDocRuleCode :: TestTree
testDocRuleCode = testCase "docRuleCode" $
    targetDocRuleCode @=? run sourceDocRuleCode

------------------------------------------------------------------------------

sourceEndDoc :: Text
sourceEndDoc = TL.unlines
    [ "// Test"
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
    [ "//"
    , "//"
    ]

targetEndDocBlanks :: Text
targetEndDocBlanks = ""

testEndDocBlanks :: TestTree
testEndDocBlanks = testCase "endDocBlanks" $
    targetEndDocBlanks @=? run sourceEndDocBlanks

------------------------------------------------------------------------------

sourceEndCode :: Text
sourceEndCode = TL.unlines
    [ "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    ]

targetEndCode :: Text
targetEndCode = TL.unlines
    [ "``` {.rust .numberSource startFrom=\"1\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , "```"
    ]

testEndCode :: TestTree
testEndCode = testCase "endCode" $
    targetEndCode @=? run sourceEndCode

------------------------------------------------------------------------------

sourceEndCodeBlanks :: Text
sourceEndCodeBlanks = TL.unlines
    [ "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , ""
    , ""
    ]

targetEndCodeBlanks :: Text
targetEndCodeBlanks = TL.unlines
    [ "``` {.rust .numberSource startFrom=\"1\"}"
    , "fn main() {"
    , "    println!(\"Hello!\");"
    , "}"
    , "```"
    ]

testEndCodeBlanks :: TestTree
testEndCodeBlanks = testCase "endCodeBlanks" $
    targetEndCodeBlanks @=? run sourceEndCodeBlanks

------------------------------------------------------------------------------

sourceEmpty :: Text
sourceEmpty = ""

sourceOnlyRule :: Text
sourceOnlyRule = "///////////////////////////////////////////////////////////"

sourceOnlyDocBlanks :: Text
sourceOnlyDocBlanks = TL.unlines
    [ "//"
    , "//"
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
tests = testGroup "DoubleSlash"
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
