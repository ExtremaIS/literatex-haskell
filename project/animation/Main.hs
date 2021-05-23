module Main (main) where

-- https://hackage.haskell.org/package/diagrams-cairo
import Diagrams.Backend.Cairo.CmdLine

-- https://hackage.haskell.org/package/diagrams-lib
import Diagrams.Prelude

------------------------------------------------------------------------------

langs :: [String]
langs =
    [ "Bash"
    , "C"
    , "Clojure"
    , "Erlang"
    , "Haskell"
    , "Idris"
    , "JavaScript"
    , "Python"
    , "Racket"
    , "SQL"
    , "..."
    ]

------------------------------------------------------------------------------

animation :: [(Diagram B, Int)]
animation = (mkFrame "X", 300) : [(mkFrame lang, 50) | lang <- langs]
  where
    mkFrame :: String -> Diagram B
    mkFrame lang
      = baselineText ("Literate" ++ lang)
          # font "Noto Sans"
          # fontSizeL 0.5
          # fc black
          # translateX (-3)
          # translateY (-0.25)
      <> rect 6.5 1 # lw none # fc white

------------------------------------------------------------------------------

main :: IO ()
main = mainWith animation
