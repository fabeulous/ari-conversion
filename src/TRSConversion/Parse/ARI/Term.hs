{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : TRSConversion.Parse.Problem.Term
-- Description : Term, function symbol, and variable parser
--
-- This module defines functions to parse terms from a @String@ input.
-- Also specifies the allowed tokens for function symbols and variables.
module TRSConversion.Parse.ARI.Term
  ( -- * ARI Terms
    parsePrefixTerm,

    -- * Functions and Variables
    parseFunSymbol,
    parseVariable,
  )
where

import TRSConversion.Parse.Utils (Parser, lexeme)
import TRSConversion.Problem.Common.Term (Term (..))
import TRSConversion.Problem.Trs.Sig (Sig (..))
import Data.List (find)
import Data.Text (Text)
import Text.Megaparsec (Tokens, anySingle, between, choice, lookAhead, many, notFollowedBy, sepEndBy, some, try, (<?>), (<|>))
import Text.Megaparsec.Char (char, spaceChar, string)


-- | Parses a single variable name using 'parseCopsSym' and returns a string.
--
-- Does not consume trailing whitespace (needed for parsing terms in prefix notation).
parseVariable :: Parser String
parseVariable = parseCopsSym <?> "variable"

-- | Parses a function symbol either until the first '(' or as long as characters allowed by 'parseCopsSym' are present.
-- Does not consume the final @'('@ and does not consume all input.
--
-- This function doesn't consume trailing whitespace (needed for parsing terms in prefix notation).
parseFunSymbol :: Parser String
parseFunSymbol =
  try (parseCopsSym <* lookAhead (char '('))
    <|> parseCopsSym
    <?> "function symbol"

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
