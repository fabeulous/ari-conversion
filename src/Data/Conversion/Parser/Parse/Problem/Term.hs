{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.Conversion.Parser.Parse.Problem.Term
-- Description : Term, function symbol, and variable parser
--
-- This module defines functions to parse terms from a @String@ input.
module Data.Conversion.Parser.Parse.Problem.Term
  ( parseTerm,
    parseVariable,
    parseFunSymbol,
    parsePrefixTerm,
  )
where

import Control.Monad (guard)
import Data.Conversion.Parser.Parse.Utils (Parser, lexeme, stripSpaces, symbol)
import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Data.List (find)
import Text.Megaparsec
  ( between,
    choice,
    eof,
    lookAhead,
    many,
    noneOf,
    sepBy,
    sepEndBy,
    some,
    try,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char (char, letterChar, spaceChar)

-- | Type synonym for a list of variables
type Vars = [String]

-- | Parses a term given a list of variables by calling 'parseVariable' and 'parseFunApplication'.
-- Expects a term in applicative notation (e.g. @f(x,y)@) rather than in prefix notation.
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
      input <- lexeme parseFunSymbol
      return (Fun input [])

-- | Parses a single variable name and returns a string.
-- Currently requires the first character to be a letter.
--
-- Does not consume trailing whitespace (needed for parsing terms in prefix notation).
-- qqjf update to the correct criteria.
parseVariable :: Parser String
parseVariable = (:) <$> letterChar <*> many allowedFunVarChars <?> "variable"

-- | Parses a function application and returns a 'Term' by assuming that everything
-- until the first @'('@ is a function symbol.
-- For example, @"f(x)"@ should be parsed as @Fun "f" [Var "x"]@ and
--  @"f()"@ should be parsed as a constant @Fun "f" []@.
parseFunApplication :: Vars -> Parser (Term String String)
parseFunApplication vs = do
  fsym <- lexeme parseFunSymbol
  args <- symbol "(" *> parseFunArgs vs <* symbol ")"
  return (Fun fsym args)

-- | Parses a function symbol either until the first '(' or as long as characters in 'allowedFunVarChars' are present.
-- Does not consume the final @'('@ and does not consume all input.
-- This function doesn't consume trailing whitespace (needed for parsing terms in prefix notation).
parseFunSymbol :: Parser String
parseFunSymbol =
  try
    (funSymChars <* lookAhead (char '('))
    <|> funSymChars
    <?> "function symbol"
  where
    funSymChars :: Parser String
    funSymChars = some allowedFunVarChars

-- | Parses a comma-separated list of function arguments and returns a list of the arguments.
-- Also accepts an empty string or just whitespace, corresponding to an empty arguments list.
-- e.g. if @"x"@ is in the variable list and @"a"@ is not then "a, x" should be parsed as
-- [Fun "a" [], Var "x"]
parseFunArgs :: Vars -> Parser [Term String String]
parseFunArgs vs = parseTerm vs `sepBy` char ',' <?> "function arguments"

-- Strip outer parentheses. Consumes the entire input.
outerParens :: Parser a -> Parser a
outerParens = between (symbol "(") (symbol ")" *> eof)

-- | Parser for characters allowed after the first character of variables and for function symbols.
--   qqjf: block all whitespace and special characters, not just a single space
--   qqjf: Currently forbids '-' as this might clash with "->" in rule definitions
-- qqjf COPS: vars can not contain whitespace,  (   )   "   ,   |   \ and the sequences  ->   ==   COMMENT   VAR   RULES
allowedFunVarChars :: Parser Char
allowedFunVarChars = noneOf ['(', ')', ' ', ',', '-', '\n']

-- | Parses a term in prefix notation.
-- Tries to parse the expression as a function application first, then as a constant, and finally as a variable.
--
-- This order of priority is important as 'parsePrefixTerm' does not consume the entire input.
-- For example, @f x@ should be parsed as @Fun "f" [Var "x"]@, rather than as a constant @f@ with trailing input @x@.
-- Note that this order of precedence is reversed compared to 'parseTerm' as the function
-- signature is given rather than a list of variables for ARI format.
--
-- Consumes trailing white space only after the term has been recursively parsed as far as possible.
parsePrefixTerm :: [Sig String] -> Parser (Term String String)
parsePrefixTerm funSig = lexeme $ recursivelyParsePrefixTerm funSig

-- | Helper function for the recursive steps of ''.
-- This function strips only leading spaces as arguments are separated by whitespace  in prefix notation.
recursivelyParsePrefixTerm :: [Sig String] -> Parser (Term String String)
recursivelyParsePrefixTerm funSig =
  many spaceChar
    *> choice
      [ char '(' *> parsePrefixTerm funSig <* char ')', -- Important not to strip whitespace here
        try (parsePrefixFunApplication funSig) <|> try parseVar
      ]
  where
    -- Parse a single variable and assert that it is a member of the variable set @vs@
    parseVar :: Parser (Term f String)
    parseVar = Var <$> parseVariable <?> "variable"

-- | Parses a function application in prefix notation and returns a 'Term' by assuming that
-- everything not contained in @funSig@ is a variable.
--
-- For example, @"f x y"@ should be parsed as @Fun "f" [Var "x", Var "y"]@ and
--  @"f"@ should be parsed as a constant @Fun "f" []@.
parsePrefixFunApplication :: [Sig String] -> Parser (Term String String)
parsePrefixFunApplication funSig = do
  (fsym, Sig _ arity) <- funSymInSignature
  if arity == 0
    then return $ Fun fsym [] -- Special case for constants (no arguments)
    else do
      args <- recursivelyParsePrefixTerm funSig `sepEndBy` some spaceChar <?> "function arguments"
      return $ Fun fsym args
  where
    -- Parse a single function symbol and assert that it is in the function signature @funSig@.
    -- Returns a tuple of the function symbol and first signature in @funSig@ with a matching function symbol.
    funSymInSignature :: Parser (String, Sig String)
    funSymInSignature = do
      fsym <- parseFunSymbol
      case find (\(Sig f _) -> f == fsym) funSig of
        Just sig -> return (fsym, sig)
        Nothing -> fail $ "Function symbol " ++ fsym ++ " not in signature " ++ show funSig
