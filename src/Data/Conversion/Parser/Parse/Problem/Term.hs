{-# LANGUAGE OverloadedStrings #-}

module Data.Conversion.Parser.Parse.Problem.Term
  ( parseTerm,
    parseVariable,
    parseFunSymbol,
  )
where

import Control.Monad (guard)
import Data.Conversion.Parser.Parse.Utils (Parser, lexeme, stripSpaces, symbol)
import Data.Conversion.Problem.Term (Term (..))
import Text.Megaparsec
  ( between,
    choice,
    eof,
    lookAhead,
    many,
    noneOf,
    sepBy,
    some,
    try,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char (char, letterChar)

type Vars = [String]

-- | Parse a term given a list of variables by calling 'parseVariable' and 'parseFunApplication'
-- Tries to parse the expression as a variable first
parseTerm :: Vars -> Parser (Term String String)
parseTerm vs =
  stripSpaces $
    choice
      [ outerParens (parseTerm vs),
        parseTermHelper
      ]
  where
    -- \| Try to parse the given string as a variable, then as a function application, then as a constant
    parseTermHelper :: Parser (Term String String)
    parseTermHelper =
      try parseVar
        <|> try (parseFunApplication vs)
        <|> try parseConstant
    -- \| Parse a single variable and require that it is a member of the variable set @vs@
    parseVar :: Parser (Term f String)
    parseVar = do
      varStr <- parseVariable
      guard (varStr `elem` vs)
      return (Var varStr)
    parseConstant :: Parser (Term String String)
    parseConstant = do
      input <- parseFunSymbol
      return (Fun input [])

-- | Strip outer parentheses
outerParens :: Parser a -> Parser a
outerParens = between (symbol "(") (symbol ")" *> eof)

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
