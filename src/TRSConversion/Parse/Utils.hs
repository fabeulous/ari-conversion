{- |
Module      : TRSConversion.Parse.Utils
Description : Utils for parsing with Megaparsec

This module defines a type synonym 'Parser', whitespace helpers, and other helpers functions to aid
parsing with 'Megaparsec'.
-}
module TRSConversion.Parse.Utils (
  -- * Types
  Parser,
  -- * Parsing
  parseIO,
)
where

import Data.Text (Text)
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, errorBundlePretty, parse)
import Text.Megaparsec.Error (ShowErrorComponent)

{- | Type alias for a 'Megaparsec' parser which uses error handler of type 'Void' and
takes an input of type 'Text'.
-}
type Parser e = Parsec e Text

parseIO :: ShowErrorComponent e => Parser e a -> String -> Text -> IO a
parseIO p inpName inp =
  case parse p inpName inp of
    Left err -> do
      putStrLn "Error: invalid input"
      putStr $ errorBundlePretty err
      exitFailure
    Right trs -> return trs
