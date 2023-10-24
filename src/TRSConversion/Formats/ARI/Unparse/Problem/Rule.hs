{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : TRSConversion.Formats.ARI.Unparse.Problem.Rule
-- Description : Unparser for rewrite rules
--
-- This module defines functions to unparse single TRS 'Rule's and blocks of rules
-- in COPS and ARI format.
module TRSConversion.Formats.ARI.Unparse.Problem.Rule
  ( -- * ARI
    unparseAriSystems,
    unparseAriRules,
    unparseAriRule,
  )
where

import Prettyprinter (Doc, Pretty, parens, pretty, vsep, (<+>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import TRSConversion.Problem.Common.Rule (Rule (..))
import TRSConversion.Formats.ARI.Unparse.Problem.Term (unparsePrefixTerm)

-- | Unparse an indexed map of trss.
unparseAriSystems :: (Pretty f, Pretty v) => IntMap [Rule f v] -> Doc ann
unparseAriSystems systems =
  vsep $ map (uncurry unparseAriRules) (IntMap.toList systems)

-- | Unparse a list of 'Rule's into [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/trs.txt)
-- separated by newlines. Uses 'unparseAriRule' to parse individual rules.
--
-- If no rules are given, returns @Nothing@ and otherwise returns @Just rulesBlock@.
unparseAriRules :: (Pretty f, Pretty v) => Int -> [Rule f v] -> Doc ann
unparseAriRules index rs = vsep (map (unparseAriRule index) rs)

-- | Unparse a 'Rule' into the format expected by ARI. Uses 'unparsePrefixTerm' to
-- unparse each side of the rule into prefix format.
--
-- >>> unparseAriRule $ Rule {lhs=Fun "f" [Var "x", Fun "a" []], rhs=Var "x"}
-- (rule (f x a) x)
--
-- >>> unparseAriRule $ Rule {lhs=Fun "f" [Var "x", Fun "g" [Var "y"]], rhs=Fun "g" [Var "y"]}
-- (rule (f x (g y)) (g y))
unparseAriRule :: (Pretty f, Pretty v) => Int -> Rule f v -> Doc ann
unparseAriRule i (Rule l r) =
  parens $
    "rule" <+> unparsePrefixTerm l <+> unparsePrefixTerm r <>
      if i > 1 then mempty <+> ":index" <+> pretty i else mempty


-- $setup
-- >>> import TRSConversion.Problem.Common.Term
