module LiterateX.Test.SourceFormat (tests) where

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- (literatex:test)
import qualified LiterateX.Test.SourceFormat.DoubleDash
import qualified LiterateX.Test.SourceFormat.DoubleSlash
import qualified LiterateX.Test.SourceFormat.Hash
import qualified LiterateX.Test.SourceFormat.LispSemicolons
import qualified LiterateX.Test.SourceFormat.LiterateHaskell
import qualified LiterateX.Test.SourceFormat.Percent

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "LiterateX.Test.SourceFormat"
    [ LiterateX.Test.SourceFormat.DoubleDash.tests
    , LiterateX.Test.SourceFormat.DoubleSlash.tests
    , LiterateX.Test.SourceFormat.Hash.tests
    , LiterateX.Test.SourceFormat.LispSemicolons.tests
    , LiterateX.Test.SourceFormat.LiterateHaskell.tests
    , LiterateX.Test.SourceFormat.Percent.tests
    ]
