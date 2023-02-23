{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Conversion.Problem.Term.Parse
  ( parseTerm,
    parseVariable,
    parseFunSymbol,
  )
where

import Control.Monad (guard)
import Data.Conversion.Utils (lexeme, sc)
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
    lookAhead,
    many,
    manyTill_,
    noneOf,
    notFollowedBy,
    parse,
    sepBy,
    some,
    try,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char (char, letterChar, spaceChar)
import qualified Text.Megaparsec.Char.Lexer as L

symbol :: Text -> Parser Text
symbol = L.symbol sc

type Parser = Parsec Void Text

type Vars = [String]

-- | Parse a term given a list of variables by calling 'parseVariable' and 'parseFunApplication'
-- Tries to parse the expression as a variable first
parseTerm :: Vars -> Parser (Term String String)
parseTerm vs =
  stripSpaces $
    choice
      [ parens (parseTerm vs),
        parseTermHelper
      ]
  where
    -- \| Try to parse the given string as a variable, then as a function application, then as a constant
    parseTermHelper :: Parser (Term String String)
    parseTermHelper =
      try (parseVar vs)
        <|> try (parseFunApplication vs)
        <|> try parseConstant
    -- \| Parse a single variable and require that it is a member of the variable set @vs@
    parseVar :: Vars -> Parser (Term f String)
    parseVar vs = do
      varStr <- parseVariable
      guard (varStr `elem` vs)
      return (Var varStr)
    parseConstant :: Parser (Term String String)
    parseConstant = do
      input <- parseFunSymbol
      return (Fun input [])

-- | Strip outer parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")" *> eof)

-- | Parse a single variable name and returns a string
--   Currently requires the first character to be a letter and comsumes trailing whitespace
parseVariable :: Parser String
parseVariable = (:) <$> letterChar <*> lexeme (many allowedFunVarChars) <?> "variable"

-- | Parse a function application and return a 'Term'
-- Assumes that everything until the first @'('@ is a function symbol
-- For example, "f(x,y, g(z))"
parseFunApplication :: Vars -> Parser (Term String String)
parseFunApplication vs =
  do
    fsym <- parseFunSymbol
    args <- symbol "(" *> parseFunArgs vs <* symbol ")" -- <* notFollowedBy (symbol ")")
    return (Fun fsym args)

-- | Strip spaces at start and end of a string
stripSpaces :: Parser a -> Parser a
stripSpaces p = lexeme (many spaceChar *> p)

-- | Recursively strip outer parentheses of a function application, even if nested
-- Done in a slightly weird way with megaparsec as this library expects to always process streams from front to back
stripOuterParens :: Parser String
stripOuterParens = do
  out <- aux
  let output = case parse stripOuterParens "" (pack out) of
        Left _ -> out -- Original output was ok
        Right xs -> xs
  return output
  where
    aux :: Parser String
    aux = do
      (noParens, _) <- char '(' *> manyTill_ anySingle (try (lexeme (char ')') <* eof))
      return noParens

-- | Parse a function symbol either until the first '(' or as long as allowed characters are there
-- Important: does not consume all input
parseFunSymbol :: Parser String
parseFunSymbol =
  try
    (funSymChars <* lookAhead (char '('))
    <|> funSymChars
    <?> "function symbol"
  where
    funSymChars :: Parser String
    funSymChars = lexeme (some allowedFunVarChars)

-- | Parse function arguments
-- Expects balanced parentheses and comma-separated values
-- Returns a list of the arguments. Also accepts an empty string or just whitespace, corresponding to an empty arguments list.
-- e.g. "(a, b, c)" or "(a, b(c))"
parseFunArgs :: Vars -> Parser [Term String String]
parseFunArgs vs = parseTerm vs `sepBy` char ',' <?> "function arguments"

-- | Parser for characters allowed after the first character of variables and for function symbols.
--   Currently allows any character except for '(', ')', ',', and whitespace.
--   TODO: block all whitespace and special characters, not just a single space
--   Currently forbids '-' as this might clash with "->" in rule definitions
-- COPS: vars can not contain whitespace,  (   )   "   ,   |   \ and the sequences  ->   ==   COMMENT   VAR   RULES
allowedFunVarChars :: Parser Char
allowedFunVarChars = noneOf ['(', ')', ' ', ',', '-', '\n']
