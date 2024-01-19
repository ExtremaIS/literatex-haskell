------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : literatex command-line utility
-- Copyright   : Copyright (c) 2021-2024 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

#if defined(MIN_VERSION_ansi_wl_pprint)
#if MIN_VERSION_ansi_wl_pprint (1,0,2)
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
#endif
#endif

module Main (main) where

-- https://hackage.haskell.org/package/base
import Control.Applicative (optional)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr, stdin, stdout)

-- https://hackage.haskell.org/package/optparse-applicative
import qualified Options.Applicative as OA

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

-- (literatex)
import qualified LiterateX
import qualified LiterateX.Renderer as Renderer
import qualified LiterateX.SourceDefaults as SourceDefaults
import LiterateX.Types (CodeLanguage, SourceFormat, TargetFormat)
import qualified LiterateX.Types.SourceFormat as SourceFormat
import qualified LiterateX.Types.TargetFormat as TargetFormat

-- (literatex:executable)
import qualified LibOA

------------------------------------------------------------------------------
-- $Defaults

defaultTargetFormat :: TargetFormat
defaultTargetFormat = TargetFormat.PandocMarkdown

------------------------------------------------------------------------------
-- $Options

data Options
  = Options
    { formatOpt      :: !(Maybe SourceFormat)
    , rendererOpts   :: !Renderer.Options
    , noHighlightOpt :: !Bool
    , inputOpt       :: !(Maybe FilePath)
    , outputOpt      :: !(Maybe FilePath)
    }

sourceFormatOption :: OA.Parser SourceFormat
sourceFormatOption = OA.option (OA.eitherReader TTC.parse) $ mconcat
    [ OA.long "source"
    , OA.short 's'
    , OA.metavar "SOURCE"
    , OA.help "source format"
    ]

targetFormatOption :: OA.Parser TargetFormat
targetFormatOption = OA.option (OA.eitherReader TTC.parse) $ mconcat
    [ OA.long "target"
    , OA.short 't'
    , OA.metavar "TARGET"
    , OA.value defaultTargetFormat
    , OA.showDefaultWith TTC.render
    , OA.help "target format"
    ]

languageOption :: OA.Parser CodeLanguage
languageOption = OA.option (OA.eitherReader TTC.parse) $ mconcat
    [ OA.long "language"
    , OA.short 'l'
    , OA.metavar "LANGUAGE"
    , OA.help "code language"
    ]

ignoreShebangOption :: OA.Parser Bool
ignoreShebangOption = OA.switch $ mconcat
    [ OA.long "ignore-shebang"
    , OA.help "ignore shebangs"
    ]

noCodeOption :: OA.Parser Bool
noCodeOption = OA.switch $ mconcat
    [ OA.long "no-code"
    , OA.help "do not display code"
    ]

noNumbersOption :: OA.Parser Bool
noNumbersOption = OA.switch $ mconcat
    [ OA.long "no-numbers"
    , OA.help "do not number code lines"
    ]

renderOptions :: OA.Parser Renderer.Options
renderOptions = Renderer.Options
    <$> targetFormatOption
    <*> optional languageOption
    <*> ignoreShebangOption
    <*> fmap not noCodeOption
    <*> fmap not noNumbersOption

noHighlightOption :: OA.Parser Bool
noHighlightOption = OA.switch $ mconcat
    [ OA.long "no-highlight"
    , OA.help "do not highlight code"
    ]

inputOption :: OA.Parser FilePath
inputOption = OA.strOption $ mconcat
    [ OA.long "input"
    , OA.short 'i'
    , OA.metavar "FILE"
    , OA.help "input file (default: STDIN)"
    ]

outputOption :: OA.Parser FilePath
outputOption = OA.strOption $ mconcat
    [ OA.long "output"
    , OA.short 'o'
    , OA.metavar "FILE"
    , OA.help "output file (default: STDOUT)"
    ]

options :: OA.Parser Options
options = Options
    <$> optional sourceFormatOption
    <*> renderOptions
    <*> noHighlightOption
    <*> optional inputOption
    <*> optional outputOption

------------------------------------------------------------------------------
-- $Library

errorExit :: String -> IO a
errorExit message = do
    hPutStrLn stderr $ "error: " ++ message
    exitWith $ ExitFailure 1

------------------------------------------------------------------------------
-- $Main

main :: IO ()
main = do
    Options{..} <- OA.execParser pinfo
    let defaultOpts = SourceDefaults.defaultsFor =<< inputOpt
    format <- case (formatOpt, defaultOpts, inputOpt) of
      (Just format, _, _) -> pure format
      (Nothing, Just (format, _), _) -> pure format
      (Nothing, Nothing, Just{}) ->
        errorExit "source format not specified and unknown input filename"
      (Nothing, Nothing, Nothing) ->
        errorExit "source format not specified and no input filename"
    let codeLanguage = Renderer.codeLanguage rendererOpts
        opts = rendererOpts
          { Renderer.codeLanguage =
              case (noHighlightOpt, codeLanguage, defaultOpts) of
                (True, _, _) -> Nothing
                (False, opt@Just{}, _) -> opt
                (False, Nothing, Just (_, lang)) -> Just lang
                (False, Nothing, Nothing) -> Nothing
          }
    case (inputOpt, outputOpt) of
      (Just input, Just output) ->
        LiterateX.transformFileToFile format opts input output
      (Just input, Nothing) ->
        LiterateX.transformFileToHandle format opts input stdout
      (Nothing, Just output) ->
        LiterateX.transformHandleToFile format opts stdin output
      (Nothing, Nothing) ->
        LiterateX.transformHandleToHandle format opts stdin stdout
  where
    pinfo :: OA.ParserInfo Options
    pinfo
      = OA.info
          (LibOA.helper <*> LibOA.versioner LiterateX.version <*> options)
      $ mconcat
          [ OA.fullDesc
          , OA.progDesc "transform literate source code to Markdown"
          , OA.failureCode 2
          , OA.footerDoc . Just $ LibOA.vspace
              [ sourceFormatHelp
              , targetFormatHelp
              , defaultsHelp
              ]
          ]

    sourceFormatHelp :: LibOA.Doc
    sourceFormatHelp = LibOA.section "SOURCE options:" $ LibOA.table_ 2
      [ [TTC.render format, SourceFormat.describe format]
      | format <- SourceFormat.list
      ]

    targetFormatHelp :: LibOA.Doc
    targetFormatHelp = LibOA.section "TARGET options:" $ LibOA.table_ 2
      [ [TTC.render format, TargetFormat.describe format]
      | format <- TargetFormat.list
      ]

    defaultsHelp :: LibOA.Doc
    defaultsHelp = LibOA.section "Default options:" $ LibOA.table_ 2
      [ [ext, TTC.render lang ++ " (" ++ TTC.render format ++ ")"]
      | (ext, (format, lang)) <- SourceDefaults.extensionDefaults
      ]
