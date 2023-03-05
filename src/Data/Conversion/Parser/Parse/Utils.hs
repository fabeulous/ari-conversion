{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.Conversion.Parser.Parse.Utils
-- Description : Utils for parsing with Megaparsec
--
-- This module defines a type synonym 'Parser', whitespace helpers, and reusable helpers to aid parsing with 'Megaparsec'.
module Data.Conversion.Parser.Parse.Utils
  ( -- * Types
    Parser,

    -- * Whitespace Helpers
    sc,
    lexeme,
    stripSpaces,

    -- * Other Helpers
    parens,
    symbol,
  )
where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, many)
import Text.Megaparsec.Char (space1, spaceChar)
import qualified Text.Megaparsec.Char.Lexer as L

-- | Type alias for a 'Megaparsec' parser which uses error handler of type 'Void' and
-- takes an input of type 'Text'.
type Parser = Parsec Void Text

-- | Custom space handler. See [the Megaparsec docs](https://hackage.haskell.org/package/megaparsec-9.3.0/docs/Text-Megaparsec-Char-Lexer.html#g:1)
-- for more information.
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

-- | 'lexeme' is a [Megaparsec lexeme](https://hackage.haskell.org/package/megaparsec-9.3.0/docs/Text-Megaparsec-Char-Lexer.html#v:lexeme)
-- that consumes all trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Strips spaces at the start and end of a string.
stripSpaces :: Parser a -> Parser a
stripSpaces p = lexeme (many spaceChar *> p)

-- | 'symbol' is a [Megaparsec symbol](https://hackage.haskell.org/package/megaparsec-9.3.0/docs/Text-Megaparsec-Char-Lexer.html#v:symbol)
-- that parses 'Text', wrapping it in a 'Parser' and consuming trailing whitespace.
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Takes a given a given parser and applies it [between](https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Monad-Combinators.html#v:between)
-- parentheses.
parens :: Parser a -> Parser a
parens = between (lexeme $ symbol "(") (lexeme $ symbol ")")
