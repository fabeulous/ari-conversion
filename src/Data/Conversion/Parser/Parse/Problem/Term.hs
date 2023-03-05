{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.Conversion.Parser.Parse.Problem.Term
-- Description : Term, function symbol, and variable parser
--
-- This module defines functions 'parseTerm', 'parseVariable', and 'parseFunSymbol' to parse terms from a @String@ input.
module Data.Conversion.Parser.Parse.Problem.Term
  ( parseTerm,
    parseVariable,
    parseFunSymbol,
  )
where

import Control.Monad (guard)
import Data.Conversion.Parser.Parse.Utils (Parser, lexeme, stripSpaces, symbol)
import Data.Conversion.Problem.Common.Term (Term (..))
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

-- | Type synonym for a list of variables
type Vars = [String]

-- | Parses a term given a list of variables by calling 'parseVariable' and 'parseFunApplication'.
-- Tries to parse the expression as a variable first, then as a function application, and finally as a constant.
--
-- This order of priority is important as 'parseTerm' does not consume the entire input.
-- For example, @f(x)@ should be parsed as @Fun "f" [Var "x"]@, rather than as a constant @f@ with trailing input @(x)@.
parseTerm :: Vars -> Parser (Term String String)
parseTerm vs =
  stripSpaces $
    choice
      [ outerParens (parseTerm vs),
        try parseVar <|> try (parseFunApplication vs) <|> try parseConstant
      ]
  where
    -- Parse a single variable and assert that it is a member of the variable set @vs@
    parseVar :: Parser (Term f String)
    parseVar = do
      varStr <- parseVariable
      guard (varStr `elem` vs)
      return (Var varStr)
    -- Parse a constant (must be called after trying to parse input as a function application)
    parseConstant :: Parser (Term String String)
    parseConstant = do
      input <- parseFunSymbol
      return (Fun input [])
    -- Strip outer parentheses
    outerParens :: Parser a -> Parser a
    outerParens = between (symbol "(") (symbol ")" *> eof)

-- | Parses a single variable name and returns a string.
-- Currently requires the first character to be a letter and comsumes trailing whitespace.
-- qqjf update to the correct criteria.
parseVariable :: Parser String
parseVariable = (:) <$> letterChar <*> lexeme (many allowedFunVarChars) <?> "variable"

-- | Parses a function application and returns a 'Term' by assuming that everything
-- until the first @'('@ is a function symbol.
-- For example, @"f(x)"@ should be parsed as @Fun "f" [Var "x"]@ and
--  @"f()"@ should be parsed as a constant @Fun "f" []@.
parseFunApplication :: Vars -> Parser (Term String String)
parseFunApplication vs = do
  fsym <- parseFunSymbol
  args <- symbol "(" *> parseFunArgs vs <* symbol ")"
  return (Fun fsym args)

-- | Parses a function symbol either until the first '(' or as long as characters in 'allowedFunVarChars' are present.
-- Does not consume the final @'('@ and does not consume all input.
parseFunSymbol :: Parser String
parseFunSymbol =
  try
    (funSymChars <* lookAhead (char '('))
    <|> funSymChars
    <?> "function symbol"
  where
    funSymChars :: Parser String
    funSymChars = lexeme (some allowedFunVarChars)

-- | Parses a comma-separated list of function arguments and returns a list of the arguments. 
-- Also accepts an empty string or just whitespace, corresponding to an empty arguments list.
-- e.g. if @"x"@ is in the variable list and @"a"@ is not then "a, x" should be parsed as
-- [Fun "a" [], Var "x"]
parseFunArgs :: Vars -> Parser [Term String String]
parseFunArgs vs = parseTerm vs `sepBy` char ',' <?> "function arguments"

-- | Parser for characters allowed after the first character of variables and for function symbols.
--   qqjf: block all whitespace and special characters, not just a single space
--   qqjf: Currently forbids '-' as this might clash with "->" in rule definitions
-- qqjf COPS: vars can not contain whitespace,  (   )   "   ,   |   \ and the sequences  ->   ==   COMMENT   VAR   RULES
allowedFunVarChars :: Parser Char
allowedFunVarChars = noneOf ['(', ')', ' ', ',', '-', '\n']
