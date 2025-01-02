------------------------------------------------------------------------------
-- |
-- Module      : LiterateX.Types.SourceLine
-- Description : source line type
-- Copyright   : Copyright (c) 2021-2025 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

module LiterateX.Types.SourceLine
  ( -- * Type
    SourceLine(..)
  ) where

-- https://hackage.haskell.org/package/text
import Data.Text (Text)

------------------------------------------------------------------------------
-- $Type

-- | Parsed source line
--
-- @since 0.0.1.0
data SourceLine
  = Shebang !Text  -- ^ script shebang on first line
  | CodeBlank      -- ^ code blank line
  | DocBlank       -- ^ documentation blank line
  | Rule           -- ^ comment rule used to organize source code
  | Doc !Text      -- ^ documentation line
  | Code !Text     -- ^ code line
  deriving Show
