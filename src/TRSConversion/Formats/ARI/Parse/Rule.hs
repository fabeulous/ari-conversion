{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Parse.Problem.Rule
Description : Rule parsers

This module defines parsers to parse a single rule and a block of rules in different rewriting formats.
-}
module TRSConversion.Formats.ARI.Parse.Rule (
  -- * ARI
  parseAriRule,
)
where

import TRSConversion.Formats.ARI.Parse.Term (parsePrefixTerm)
import TRSConversion.Formats.ARI.Parse.Utils (ARIParser, sExpr, keyword, index, FunSymb, VarSymb)
import TRSConversion.Problem.Common.Rule (Rule (..))
import TRSConversion.Problem.Trs.TrsSig (Sig)
import Text.Megaparsec (option, getOffset)
import TRSConversion.Problem.Common.Index (Index)
import qualified TRSConversion.Problem.Common.Index as Index

{- | Parse a rule block consisting of two terms in prefix notation
separated by at least one space character.

Uses 'parsePrefixTerm' to parse each term and consumes leading spaces.
Also consumes spaces surrounding the term inside the parentheses.

>>> parseTest (parseAriRule [Sig "f" 1]) "(f x) (x)"
Rule {lhs=Fun "f" [Var "x"], rhs=Var "x"}

See the tests for more examples.
-}
parseAriRule :: [Sig FunSymb] -> ARIParser (Index, Rule FunSymb VarSymb)
parseAriRule funSig = sExpr "rule" $ do
  l <- parsePrefixTerm funSig
  r <- parsePrefixTerm funSig
  o <- getOffset
  n <- option (Index.Index 1 o) $ keyword ":index" >> index
  return $ (n, Rule{lhs = l, rhs = r})