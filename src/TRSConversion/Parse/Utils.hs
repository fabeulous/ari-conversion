{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : TRSConversion.Parse.Utils
-- Description : Utils for parsing with Megaparsec
--
-- This module defines a type synonym 'Parser', whitespace helpers, and other helpers functions to aid
-- parsing with 'Megaparsec'.
module TRSConversion.Parse.Utils
  ( -- * Types
    Parser,
  )
where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)

-- | Type alias for a 'Megaparsec' parser which uses error handler of type 'Void' and
-- takes an input of type 'Text'.
type Parser = Parsec Void Text

