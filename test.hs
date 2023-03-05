{-# LANGUAGE OverloadedStrings #-}

module Data.Conversion.Testt
  (  
  )
where
{-
import Control.Monad (guard)
import Data.Rewriting.Term.Type
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    anySingle,
    between,
    choice,
    eof,
    errorBundlePretty,
    fancyFailure,
    lookAhead,
    many,
    manyTill_,
    noneOf,
    parse,
    parseTest,
    satisfy,
    sepBy,
    skipMany,
    some,
    try,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char (char, letterChar, space1, spaceChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ErrorFancy (..), errorBundlePretty)

type Parser = Parsec Void Text

type Vars = [String]

t0 = parseTest (parseTerm ["x", "y"] <* eof) "f(c,y,z)"-}