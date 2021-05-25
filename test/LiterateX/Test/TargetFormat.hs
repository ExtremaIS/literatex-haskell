{-# LANGUAGE OverloadedStrings #-}

module LiterateX.Test.TargetFormat (tests) where

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

sourceTL :: Text
sourceTL = TL.unlines
    [ "#!/usr/bin/env python"
    , ""
    , "# Test"
    , ""
    , "print('Hello!')"
    ]

------------------------------------------------------------------------------

pandocLangNum :: Text
pandocLangNum = TL.unlines
    [ "Test"
    , ""
    , "``` {.python .numberSource startFrom=\"5\"}"
    , "print('Hello!')"
    , "```"
    ]

pandocLangNoNum :: Text
pandocLangNoNum = TL.unlines
    [ "Test"
    , ""
    , "```python"
    , "print('Hello!')"
    , "```"
    ]

pandocNoLangNum :: Text
pandocNoLangNum = TL.unlines
    [ "Test"
    , ""
    , "``` {.numberSource startFrom=\"5\"}"
    , "print('Hello!')"
    , "```"
    ]

pandocNoLangNoNum :: Text
pandocNoLangNoNum = TL.unlines
    [ "Test"
    , ""
    , "```"
    , "print('Hello!')"
    , "```"
    ]

------------------------------------------------------------------------------

githubLangNum :: Text
githubLangNum = TL.unlines
    [ "Test"
    , ""
    , "``` python startline=5"
    , "print('Hello!')"
    , "```"
    ]

githubLangNoNum :: Text
githubLangNoNum = TL.unlines
    [ "Test"
    , ""
    , "``` python"
    , "print('Hello!')"
    , "```"
    ]

githubNoLangNum :: Text
githubNoLangNum = TL.unlines
    [ "Test"
    , ""
    , "``` startline=5"
    , "print('Hello!')"
    , "```"
    ]

githubNoLangNoNum :: Text
githubNoLangNoNum = TL.unlines
    [ "Test"
    , ""
    , "```"
    , "print('Hello!')"
    , "```"
    ]

------------------------------------------------------------------------------

rendererOpts :: Renderer.Options
rendererOpts = Renderer.Options
    { Renderer.targetFormat    = TargetFormat.PandocMarkdown
    , Renderer.codeLanguage    = Just "python"
    , Renderer.ignoreShebang   = True
    , Renderer.renderCode      = True
    , Renderer.numberCodeLines = True
    }

rendererOptsGFM :: Renderer.Options
rendererOptsGFM = rendererOpts
    { Renderer.targetFormat = TargetFormat.GitHubFlavoredMarkdown
    }

run' :: Renderer.Options -> Text
run' opts = LiterateX.transformTextToText SourceFormat.Hash opts sourceTL

------------------------------------------------------------------------------

testPandocMarkdown :: TestTree
testPandocMarkdown = testGroup "PandocMarkdown"
    [ testCase "LangNum" $ pandocLangNum @=? run' rendererOpts
    , testCase "LangNoNum" $ pandocLangNoNum @=? run' rendererOpts
        { Renderer.numberCodeLines = False
        }
    , testCase "NoLangNum" $ pandocNoLangNum @=? run' rendererOpts
        { Renderer.codeLanguage = Nothing
        }
    , testCase "NoLangNoNum" $ pandocNoLangNoNum @=? run' rendererOpts
        { Renderer.codeLanguage = Nothing
        , Renderer.numberCodeLines = False
        }
    ]

------------------------------------------------------------------------------

testGitHubFlavoredMarkdown :: TestTree
testGitHubFlavoredMarkdown = testGroup "GitHubFlavoredMarkdown"
    [ testCase "LangNum" $ githubLangNum @=? run' rendererOptsGFM
    , testCase "LangNoNum" $ githubLangNoNum @=? run' rendererOptsGFM
        { Renderer.numberCodeLines = False
        }
    , testCase "NoLangNum" $ githubNoLangNum @=? run' rendererOptsGFM
        { Renderer.codeLanguage = Nothing
        }
    , testCase "NoLangNoNum" $ githubNoLangNoNum @=? run' rendererOptsGFM
        { Renderer.codeLanguage = Nothing
        , Renderer.numberCodeLines = False
        }
    ]

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "LiterateX.Test.TargetFormat"
    [ testPandocMarkdown
    , testGitHubFlavoredMarkdown
    ]
