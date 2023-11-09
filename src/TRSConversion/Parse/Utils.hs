{- |
Module      : TRSConversion.Parse.Utils
Description : Utils for parsing with Megaparsec

This module defines a type synonym 'Parser', whitespace helpers, and other helpers functions to aid
parsing with 'Megaparsec'.
-}
{-# LANGUAGE DeriveFunctor #-}
module TRSConversion.Parse.Utils (
  -- * Types
  Parser,
  Token(..),
  mkToken,
  mkToken',
  unToken,
  -- * Parsing
  parseIO,
  parseIOpartial,
)
where

import Data.Text (Text, unpack, pack)
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, errorBundlePretty, parse, eof, hidden)
import Text.Megaparsec.Error (ShowErrorComponent)
{- | Type alias for a 'Megaparsec' parser which uses error handler of type 'Void' and
takes an input of type 'Text'.
-}
type Parser e = Parsec e Text

-- | forces full consumption of the input
parseIO :: ShowErrorComponent e => Parser e a -> String -> Text -> IO a
parseIO p = parseIOpartial (p <* hidden eof)

-- | only consumes as much input as the parser does
parseIOpartial :: ShowErrorComponent e => Parser e a -> String -> Text -> IO a
parseIOpartial p inpName inp =
  case parse p inpName inp of
    Left err -> do
      putStrLn "Error: invalid input"
      putStr $ errorBundlePretty err
      exitFailure
    Right trs -> return trs


data Token a = Token { tokenValue :: a
                     , tokenOffset :: Int
                     , tokenLength :: Int
                     , tokenText :: Text
                     } deriving Functor

-- | make a Token
mkToken :: a -> Int -> Int -> Text -> Token a
mkToken value offset len str =
    Token { tokenValue = value
          , tokenOffset = offset
          , tokenLength = len
          , tokenText = str
          }

-- | make a Token where the tokenText is defined by the @Show@ instance
mkToken' :: (Show a) => a -> Int -> Int -> Token a
mkToken' value offset len =
    Token { tokenValue = value
          , tokenOffset = offset
          , tokenLength = len
          , tokenText = pack $ show value
          }

instance Eq a => Eq (Token a) where
  a == b = tokenValue a == tokenValue b

instance Ord a => Ord (Token a) where
  compare a b = compare (tokenValue a) (tokenValue b)

instance Show (Token a) where
  show = unpack . tokenText

unToken :: Token a -> a
unToken = tokenValue
