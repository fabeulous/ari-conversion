{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.Conversion.Parser.Parse.Problem.Rule
-- Description : Rule parsers
--
-- This module defines parsers 'parseRule' to parse a single rule and 'parseRules' to parse a block of rules.
module Data.Conversion.Parser.Parse.Problem.Rule
  ( parseRule,
    parseRules,
  )
where

import Data.Conversion.Parser.Parse.Problem.Term (parseTerm)
import Data.Conversion.Parser.Parse.Utils (Parser, lexeme)
import Data.Conversion.Problem.Common.Rule (Rule (..), inferRulesSignature)
import Data.Conversion.Problem.Trs.Sig (Sig)
import Data.Conversion.Problem.Trs.TrsSig (TrsSig (..))
import Text.Megaparsec (many, (<?>))
import Text.Megaparsec.Char (string)

-- | Type synonym for a list of variables
type Vars = [String]

-- | Parse a single rule of the form @lhs->rhs@ by calling 'parseTerm' on each side of the separator.
-- Takes a list of variables to know whether to interpret symbols without parentheses as variables or constants.
--
-- Ignores whitespace around the @"->"@ and does not necessarily consume all input.
--
-- >>> parseRule "f(x) -> x"
-- Rule {lhs = Fun "f" [Var "x"], rhs = Var "x"}
parseRule :: Vars -> Parser (Rule String String)
parseRule vs = do
  l <- parseTerm vs <?> "left-hand side"
  _ <- lexeme (string "->")
  r <- parseTerm vs <?> "right-hand side"
  return $ Rule {lhs = l, rhs = r}

-- | Parser to extract the rules from a @RULES@ block of the [COPS TRS](http://project-coco.uibk.ac.at/problems/trs.php) format.
-- Takes a 'TrsSig' and calls 'parseRule' 0 or more times on the input until no more rules can be parsed.
-- Does not necessarily consume all input.
--
-- * If given @Vars vs@ as a signatue, then parse rules with this variable set
-- * If given @FullSig vs fs@ as a signature, then infers a function signature from the rules using @vs@ and then checks that this inferred function signature is a subset of @fs@.
-- * If given @FunSig [qqjf]
parseRules :: TrsSig String String -> Parser [Rule String String]
parseRules trsSig = case trsSig of
  Vars vs -> do
    rules <- many (parseRule vs)
    case inferRulesSignature rules of
      Left err -> fail err
      Right _ -> return rules
  FullSig vs fs -> do
    rules <- many (parseRule vs)
    case inferRulesSignature rules of
      Left err -> fail err
      Right inferredSig -> checkSignatureSubset inferredSig fs rules
  FunSig fs -> fail "FunSig not supported yet"
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