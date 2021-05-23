module Main (main) where

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (defaultMain, testGroup)

-- (literatex)
import qualified LiterateX.Test.API
import qualified LiterateX.Test.SourceFormat
import qualified LiterateX.Test.TargetFormat

------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "test"
    [ LiterateX.Test.API.tests
    , LiterateX.Test.SourceFormat.tests
    , LiterateX.Test.TargetFormat.tests
    ]
