{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- https://hackage.haskell.org/package/base
import System.IO (stdout)

-- https://hackage.haskell.org/package/literatex
import qualified LiterateX
import qualified LiterateX.Renderer as Renderer
import qualified LiterateX.Types.SourceFormat as SourceFormat

------------------------------------------------------------------------------

demo :: String
demo = unlines
    [ "-- Demo"
    , "-- ===="
    , "--"
    , "-- This source code is embedded in the example, but it could come from"
    , "-- a database or API call."
    , ""
    , "module Main (main) where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello!\""
    ]

------------------------------------------------------------------------------

main :: IO ()
main = LiterateX.runIO
    SourceFormat.DoubleDash
    (Renderer.defaultOptionsFor "haskell")
    (LiterateX.sourceString demo)
    (LiterateX.sinkHandle stdout)
