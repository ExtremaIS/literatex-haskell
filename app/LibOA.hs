------------------------------------------------------------------------------
-- |
-- Module      : LibOA
-- Description : supplementary functions for optparse-applicative
-- Copyright   : Copyright (c) 2019-2021 Travis Cardwell
-- License     : MIT
--
-- This is a collection of functions that I often use with
-- @optparse-applicative@.  I do not feel that it is worth maintaining yet
-- another helper package on Hackage, so I just copy the code to different
-- projects as required.  If the library grows to a substantial size or others
-- with to use it, I will reconsider.
--
-- Revision: 2021-06-24
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module LibOA
  ( -- * Options
    -- $Options
    helper
  , versioner
    -- * Utilities
  , commands
    -- * Help
  , (<||>)
  , section
  , table
  , vspace
  ) where

-- https://hackage.haskell.org/package/ansi-wl-pprint
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import Text.PrettyPrint.ANSI.Leijen (Doc)

-- https://hackage.haskell.org/package/base
import qualified Data.List as List
#if !MIN_VERSION_base (4,11,0)
import Data.Monoid ((<>))
#endif

-- https://hackage.haskell.org/package/optparse-applicative
import qualified Options.Applicative as OA
#if MIN_VERSION_optparse_applicative (0,16,0)
import qualified Options.Applicative.Builder.Internal as OABI
#endif
import qualified Options.Applicative.Common as OAC
import qualified Options.Applicative.Types as OAT

------------------------------------------------------------------------------
-- $Options
--
-- Option descriptions are not capitalized.

-- | A hidden @-h@ / @--help@ option that always fails, showing the help
--
-- This is the same as 'OA.helper' except that it has a different help
-- message.
helper :: OA.Parser (a -> a)
#if MIN_VERSION_optparse_applicative (0,16,0)
helper = OA.option helpReader $ mconcat
    [ OA.short 'h'
    , OA.long "help"
    , OA.value id
    , OA.metavar ""
    , OABI.noGlobal
    , OA.noArgError (OA.ShowHelpText Nothing)
    , OA.help "show this help text"
    , OA.hidden
    ]
  where
    helpReader = do
      potentialCommand <- OAT.readerAsk
      OA.readerAbort $ OA.ShowHelpText (Just potentialCommand)
#else
helper = OA.abortOption OA.ShowHelpText $ mconcat
    [ OA.short 'h'
    , OA.long "help"
    , OA.help "show help and exit"
    , OA.hidden
    ]
#endif

-- | A hidden @--version@ option that always fails, showing the version
versioner
  :: String  -- ^ version string
  -> OA.Parser (a -> a)
versioner verStr = OA.infoOption verStr $ mconcat
    [ OA.long "version"
    , OA.help "show version and exit"
    , OA.hidden
    ]

------------------------------------------------------------------------------
-- $Utilities

-- | Get a list of commands for a parser
commands :: OA.Parser a -> [String]
commands =
    let go _ opt = case OAT.optMain opt of
           OAT.CmdReader _ cmds _ -> reverse cmds
           _otherReader           -> []
    in  concat . OAC.mapParser go

------------------------------------------------------------------------------
-- $Help

-- | Insert a blank line between two documents
(<||>) :: Doc -> Doc -> Doc
d1 <||> d2 = d1 <> Doc.line <> Doc.line <> d2
infixr 5 <||>

-- | Create a section with a title and indented body
section :: String -> Doc -> Doc
section title = (Doc.text title Doc.<$$>) . Doc.indent 2

-- | Create a two-column table
table :: [(String, String)] -> Doc
table rows =
    let width = 1 + maximum (map (length . fst) rows)
    in  Doc.vcat
          [ Doc.fillBreak width (Doc.text l) Doc.<+> Doc.text r
          | (l, r) <- rows
          ]

-- | Vertically space documents with blank lines between them
vspace :: [Doc] -> Doc
vspace = mconcat . List.intersperse (Doc.line <> Doc.line)
