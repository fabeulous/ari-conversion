{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.Conversion.Parse.Problem.Term
-- Description : Term, function symbol, and variable parser
--
-- This module defines functions to parse terms from a @String@ input.
module Data.Conversion.Parse.Problem.Term
  ( parseTerm,
    parseVariable,
    parseFunSymbol,
    parseTermF,
    parsePrefixTerm,
  )
where

import Control.Monad (guard)
import Data.Conversion.Parse.Utils (Parser, lexeme, stripSpaces, symbol)
import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Data.List (find)
import Data.Text (Text)
import Text.Megaparsec (Tokens, anySingle, between, choice, eof, lookAhead, many, notFollowedBy, sepBy, sepEndBy, some, try, (<?>), (<|>))
import Text.Megaparsec.Char (char, spaceChar, string)

-- | Type synonym for a list of variables
type Vars = [String]

-- | Type synonym for a list of function symbols
type FSyms = [String]

-- | Parses a term given a list of variables by calling 'parseVariable' and 'parseFunApplication'.
-- Expects a term in applicative notation (e.g. @f(x,y)@) rather than in prefix notation.
-- Tries to parse the expression as a variable first, then as a function application, and finally as a constant.
--
-- This order of priority is important as 'parseTerm' does not consume the entire input.
-- For example, @f(x)@ should be parsed as @Fun "f" [Var "x"]@, rather than as a constant @f@ with trailing input @(x)@.
--
-- See the tests for examples of expected input.
-- If only function symbols are known then use 'parseTermF'.
parseTerm :: Vars -> Parser (Term String String)
parseTerm vs =
  stripSpaces
    ( choice
        [ outerParens (parseTerm vs),
          try parseVarInVs <|> try (parseFunApplication vs) <|> try parseConstant
        ]
    )
    <?> "term"
  where
    -- Parse a single variable and assert that it is a member of the variable set @vs@
    parseVarInVs :: Parser (Term f String)
    parseVarInVs = do
      varStr <- parseVariable
      guard (varStr `elem` vs)
      return (Var varStr)
    -- Parse a constant (must be called after trying to parse input as a function application)
    parseConstant :: Parser (Term String String)
    parseConstant = do
      input <- lexeme parseFunSymbol
      return (Fun input [])

-- | Parses a term given a list of function symbols by calling 'parseFunApplicationF' and 'parseVar'.
--
-- Expects a term in applicative notation (e.g. @f(x,y)@) rather than in prefix notation.
-- Tries to parse the expression as a a function application first, then as a constant, and finally as a variable.
-- This order of priority is important as 'parseTermF' does not consume the entire input.
--
-- See the tests for examples of expected input.
-- If only variables are known then use 'parseTerm'.
parseTermF :: FSyms -> Parser (Term String String)
parseTermF fs =
  stripSpaces
    ( choice
        [ outerParens (parseTermF fs),
          try (parseFunApplicationF fs) <|> try parseConstantF <|> try parseVar
        ]
    )
    <?> "term"
  where
    -- Parse a constant (must be called after trying to parse input as a function application)
    parseConstantF :: Parser (Term String String)
    parseConstantF = do
      fsym <- lexeme parseFunSymbol
      guard (fsym `elem` fs)
      return (Fun fsym [])

-- | Parses a single variable name and returns a string.
-- Currently requires the first character to be a letter.
--
-- Does not consume trailing whitespace (needed for parsing terms in prefix notation).
parseVariable :: Parser String
parseVariable = parseCopsSym <?> "variable"

-- | Parses a function application and returns a 'Term' by assuming that everything
-- until the first @'('@ is a function symbol.
--
-- For example, @"f(x)"@ should be parsed as @Fun "f" [Var "x"]@ and
--  @"f()"@ should be parsed as a constant @Fun "f" []@.
--
-- If only function symbols are known then use 'parseFunApplicationF'
parseFunApplication :: Vars -> Parser (Term String String)
parseFunApplication vs = do
  fsym <- lexeme parseFunSymbol <?> "function symbol"
  args <- symbol "(" *> parseFunArgs <* symbol ")" <?> "function arguments inside parentheses"
  return (Fun fsym args)
  where
    -- 'parseFunArgs' parses a comma-separated list of function arguments and returns a list of the arguments.
    -- Also accepts an empty string or just whitespace, corresponding to an empty arguments list.
    -- e.g. if @"x"@ is in the variable list and @"a"@ is not then "a, x" should be parsed as [Fun "a" [], Var "x"]
    parseFunArgs :: Parser [Term String String]
    parseFunArgs = parseTerm vs `sepBy` char ',' <?> "comma-separated function arguments"

-- | Parses a function application and returns a 'Term' by assuming that everything
-- until the first @'('@ is a function symbol. Asserts that every function symbol is in @fs@
-- and assumes that everything else is a variable.
--
-- For example, if @"f"@ is in @fs@ then @"f(x)"@ should be parsed as @Fun "f" [Var "x"]@ and
--  @"f()"@ should be parsed as a constant @Fun "f" []@.
--
-- If only variables are known then use 'parseFunApplication'
parseFunApplicationF :: FSyms -> Parser (Term String String)
parseFunApplicationF fs = do
  fsym <- lexeme parseFunSymbol <?> "function symbol"
  guard (fsym `elem` fs)
  args <- symbol "(" *> parseFunArgsF <* symbol ")" <?> "function arguments inside parentheses"
  return (Fun fsym args)
  where
    -- 'parseFunArgsF' parses a comma-separated list of function arguments and returns a list of the arguments.
    -- Also accepts an empty string or just whitespace, corresponding to an empty arguments list.
    -- e.g. if @"a"@ is in the function symbol list and @"x"@ is not then "a, x" should be parsed as [Fun "a" [], Var "x"]
    parseFunArgsF :: Parser [Term String String]
    parseFunArgsF = parseTermF fs `sepBy` char ',' <?> "comma-separated function arguments"

-- | Parses a function symbol either until the first '(' or as long as characters allowed by 'parseCopsSym' are present.
-- Does not consume the final @'('@ and does not consume all input.
-- This function doesn't consume trailing whitespace (needed for parsing terms in prefix notation).
parseFunSymbol :: Parser String
parseFunSymbol =
  try (parseCopsSym <* lookAhead (char '('))
    <|> parseCopsSym
    <?> "function symbol"

-- Strip outer parentheses. Consumes the entire input.
outerParens :: Parser a -> Parser a
outerParens = between (symbol "(") (symbol ")" *> eof)

-- | Parser for characters allowed for variables and function symbols in COPS format.
-- Consumes input as long as allowed characters or strings are present (see examples below).
--
-- The list of allowed values comes from @id@ in the [COPS TRS grammar](http://project-coco.uibk.ac.at/problems/trs.php#basic),
-- which states that id can not contain
--     * whitespace
--     * the characters (   )   "   ,   |   \
--     * the sequences ->   ==   COMMENT   VAR   RULES
-- >>> parseTest parseCopsSym "x  "
-- "x"
--
-- >>> parseTest parseCopsSym "xs-->a variable name"
-- "xs-"

---- >>> parseTest parseCopsSym "x"
--   |
-- 1 | (y)
--   | ^
-- unexpected '('
parseCopsSym :: Parser String
parseCopsSym = some allowedChar <?> "COPS symbol"
  where
    -- 'allowedChar' succeeds and returns the first character in a stream only if it is not followed by a forbidden token
    allowedChar :: Parser Char
    allowedChar = notFollowedBy forbiddenTokenNext *> anySingle
    -- Create a list of parsers for tokens forbidden in COPS format
    forbiddenTokenNext :: Parser (Tokens Text)
    forbiddenTokenNext = lookAhead forbiddenToken <?> "COPS symbol followed by a forbidden token"
    -- 'forbiddenToken' passes only on tokens forbidden by COPS format
    forbiddenToken :: Parser (Tokens Text)
    forbiddenToken =
      choice
        ( (spaceChar *> "")
            : map string ["(", ")", "\"", ",", "|", "\\", " ", " ", "\n", "->", "== ", "COMMENT", "VAR", "RULES"]
        )

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
parsePrefixTerm funSig =
  many spaceChar
    *> choice
      [ between (char '(') (char ')') (lexeme $ parsePrefixTerm funSig), -- Strip parentheses
        try (parsePrefixFunApplication funSig) <|> try parseVar
      ]
    <?> "prefix term"

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
      args <- spaceChar *> parsePrefixTerm funSig `sepEndBy` some spaceChar <?> "function arguments"
      return $ Fun fsym args
  where
    -- Parse a single function symbol and assert that it is in the function signature @funSig@.
    -- Returns a tuple of the function symbol and first signature in @funSig@ with a matching function symbol.
    funSymInSignature :: Parser (String, Sig String)
    funSymInSignature = do
      fsym <- parseFunSymbol <?> "function symbol"
      case find (\(Sig f _) -> f == fsym) funSig of
        Just sig -> return (fsym, sig)
        Nothing -> fail $ "Function symbol " ++ fsym ++ " not in signature " ++ show funSig

-- Parse a single variable using 'parseVariable'
parseVar :: Parser (Term f String)
parseVar = Var <$> parseVariable <?> "variable"
