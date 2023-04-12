{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : TRSConversion.Parse.Problem.Term
-- Description : Term, function symbol, and variable parser
--
-- This module defines functions to parse terms from a @String@ input.
-- Also specifies the allowed tokens for function symbols and variables.
module TRSConversion.Parse.Problem.Term
  ( -- * COPS Terms
    parseTerm,
    parseTermF,

    -- * ARI Terms
    parsePrefixTerm,

    -- * Functions and Variables
    parseFunSymbol,
    parseVariable,
  )
where

import Control.Monad (guard)
import TRSConversion.Parse.Utils (Parser, lexeme, stripSpaces, symbol)
import TRSConversion.Problem.Common.Term (Term (..))
import TRSConversion.Problem.Trs.Sig (Sig (..))
import Data.List (find)
import Data.Text (Text)
import Text.Megaparsec (Tokens, anySingle, between, choice, eof, lookAhead, many, notFollowedBy, sepBy, sepEndBy, some, try, (<?>), (<|>))
import Text.Megaparsec.Char (char, spaceChar, string)

-- | Parses a term given a list of variables by calling 'parseVariable' and 'parseFunApplication'.
-- Expects a term in applicative notation (e.g. @f(x,y)@) rather than in prefix notation.
--
-- Tries to parse the expression as a variable first, then as a function application, and finally as a constant.
-- This order of priority is important as 'parseTerm' does not consume the entire input.
-- For example, @f(x)@ should be parsed as @Fun "f" [Var "x"]@, rather than as a constant @f@ with trailing input @(x)@.
--
-- See the tests for examples of expected input. If only function symbols are known then use 'parseTermF'.
--
-- >>> parseTest (parseTerm ["x"]) "f(x,c)"
-- Fun "f" [Var "x", Fun "c" []]
parseTerm :: [String] -> Parser (Term String String)
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
-- Expects a term in applicative notation (e.g. @f(x,y)@) rather than in prefix notation.
--
-- Tries to parse the expression as a a function application first, then as a constant, and finally as a variable.
-- This order of priority is important as 'parseTermF' does not consume the entire input.
--
-- See the tests for examples of expected input. If only variables are known then use 'parseTerm'.
--
-- >>> parseTest (parseTermF ["f"]) "f(x,c)"
-- Fun "f" [Var "x", Fun "c" []]
parseTermF :: [String] -> Parser (Term String String)
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

-- | Parses a single variable name using 'parseCopsSym' and returns a string.
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
-- If only function symbols are known then use 'parseFunApplicationF'.
parseFunApplication :: [String] -> Parser (Term String String)
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
-- If only variables are known then use 'parseFunApplication'.
parseFunApplicationF :: [String] -> Parser (Term String String)
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
--
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

-- | Parses a term in prefix notation (see also [S-expressions](https://en.wikipedia.org/wiki/S-expression)).
--
-- Tries to parse the expression as a function application first, then as a constant, and finally as a variable.
-- This order of priority is important as 'parsePrefixTerm' does not consume the entire input.
-- For example, @f x@ should be parsed as @Fun "f" [Var "x"]@, rather than as a constant @f@ with trailing input @x@.
--
-- Consumes trailing white space only after the term has been recursively parsed as far as possible.
--
-- >>> parseTest (parseTerm [Sig "f" 2, Sig "g" 1]) "f x (g c)"
-- Fun "f" [Var "x", Fun "g" [Fun "x" []]]
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
