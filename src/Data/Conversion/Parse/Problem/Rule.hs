{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.Conversion.Parse.Problem.Rule
-- Description : Rule parsers
--
-- This module defines parsers to parse a single rule and a block of rules.
module Data.Conversion.Parse.Problem.Rule
  ( parseCopsRule,
    parseCopsTrsRules,
    parseCopsMstrsRules,
    parseAriRule,
  )
where

import Data.Conversion.Parse.Problem.Term (parsePrefixTerm, parseTerm, parseTermF)
import Data.Conversion.Parse.Utils (Parser, lexeme)
import Data.Conversion.Problem.Common.Rule (Rule (..), inferRulesSignature)
import Data.Conversion.Problem.Mstrs.MsSig (MsSig (..))
import Data.Conversion.Problem.Trs.Sig (Sig)
import Data.Conversion.Problem.Trs.TrsSig (TrsSig (..))
import Text.Megaparsec (many, some, (<?>))
import Text.Megaparsec.Char (spaceChar, string)

-- | Type synonym for a list of variables
type Vars = [String]

-- | Parse a single rule of the form @lhs->rhs@ by calling 'parseTerm' on each side of the separator.
-- Takes a list of variables to know whether to interpret symbols without parentheses as variables or constants.
--
-- Ignores whitespace around the @"->"@ and does not necessarily consume all input.
--
-- >>> parseCopsRule "f(x) -> x"
-- Rule {lhs = Fun "f" [Var "x"], rhs = Var "x"}
parseCopsRule :: Vars -> Parser (Rule String String)
parseCopsRule vs = do
  l <- parseTerm vs <?> "left-hand side"
  _ <- lexeme (string "->")
  r <- parseTerm vs <?> "right-hand side"
  return $ Rule {lhs = l, rhs = r}

-- | Parse a rule block consisting of two terms in prefix notation
-- separated by at least one space character.
-- Uses 'parsePrefixTerm' to parse each term and consumes leading spaces.
-- Also consumes spaces surrounding the term inside the parentheses.
--
-- For example, @parseAriRule [Sig f 1]@ applied to @(f x) (x)@ should be
-- parsed as @Rule {lhs=Fun "f" [Var "x"], rhs=Var "x"}@
parseAriRule :: [Sig String] -> Parser (Rule String String)
parseAriRule funSig = do
  _ <- many spaceChar
  l <- parsePrefixTerm funSig <?> "left-hand side"
  _ <- some spaceChar
  r <- parsePrefixTerm funSig <?> "right-hand side"
  return $ Rule {lhs = l, rhs = r}

-- | Parser to extract the rules from a @RULES@ block of the [COPS TRS](http://project-coco.uibk.ac.at/problems/trs.php) format.
-- Takes a 'TrsSig' and calls 'parseCopsRule' 0 or more times on the input until no more rules can be parsed.
-- Does not necessarily consume all input.
--
-- * If given @Vars vs@ as a signatue, then parse rules with this variable set
-- * If given @FullSig vs fs@ as a signature, then infers a function signature from the rules using @vs@ and then checks that this inferred function signature is a subset of @fs@.
-- * @FunSig is not supported for the TRS format
parseCopsTrsRules :: TrsSig String String -> Parser [Rule String String]
parseCopsTrsRules trsSig = case trsSig of
  Vars vs -> do
    rules <- many (parseCopsRule vs)
    case inferRulesSignature rules of
      Left err -> fail err
      Right _ -> return rules
  FullSig vs fs -> do
    rules <- many (parseCopsRule vs)
    case inferRulesSignature rules of
      Left err -> fail err
      Right inferredSig -> checkSignatureSubset inferredSig fs rules
  FunSig _ -> fail "The COPS format does not allow only specifying a function signature"
  where
    -- 'subList' returns whether every element of the first list is contained in the second list
    subList :: Eq a => [a] -> [a] -> Bool
    subList xs ys = all (`elem` ys) xs
    -- 'checkSignatureSubset' asserts that maybeSig is contained in inferredSig
    checkSignatureSubset :: [Sig String] -> [Sig String] -> [Rule String String] -> Parser [Rule String String]
    checkSignatureSubset inferredSig funSig rs =
      if inferredSig `subList` funSig
        then return rs
        else fail $ "Inferred signature " ++ show inferredSig ++ " not contained in input signature " ++ show funSig

-- | Parser to extract the rules from a @RULES@ block of the [COPS MSTRS](http://project-coco.uibk.ac.at/problems/mstrs.php) format.
-- Takes an 'MsSig' parses 0 or more rules until no more rules can be parsed.
--
-- Does not necessarily consume all input and does not type check function applications.
--
-- Can not reuse the same logic as for (unsorted) TRSs as in that case variables are specified,
-- whereas for MSTRSs function symbols are specified.
parseCopsMstrsRules :: [MsSig String String] -> Parser [Rule String String]
parseCopsMstrsRules msSigs = do many parseMstrsCopsRule
  where
    fsyms = map (\(MsSig fsym _) -> fsym) msSigs
    parseMstrsCopsRule :: Parser (Rule String String)
    parseMstrsCopsRule = do
      l <- parseTermF fsyms <?> "left-hand side"
      _ <- lexeme (string "->")
      r <- parseTermF fsyms <?> "right-hand side"
      return $ Rule {lhs = l, rhs = r}