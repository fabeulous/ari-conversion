{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Parse.Problem.Rule
Description : Rule parsers

This module defines parsers to parse a single rule and a block of rules in different rewriting formats.
-}
module TRSConversion.Parse.ARI.Rule (
  -- * ARI
  parseAriRule,
)
where

import TRSConversion.Parse.ARI.Term (parsePrefixTerm)
import TRSConversion.Parse.Utils (Parser)
import TRSConversion.Problem.Common.Rule (Rule (..))
import TRSConversion.Problem.Trs.TrsSig (Sig)

{- | Parse a rule block consisting of two terms in prefix notation
separated by at least one space character.

Uses 'parsePrefixTerm' to parse each term and consumes leading spaces.
Also consumes spaces surrounding the term inside the parentheses.

>>> parseTest (parseAriRule [Sig "f" 1]) "(f x) (x)"
Rule {lhs=Fun "f" [Var "x"], rhs=Var "x"}

See the tests for more examples.
-}
parseAriRule :: [Sig String] -> Parser (Rule String String)
parseAriRule funSig = do
  l <- parsePrefixTerm funSig
  r <- parsePrefixTerm funSig
  return $ Rule{lhs = l, rhs = r}
