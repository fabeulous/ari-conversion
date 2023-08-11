{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : TRSConversion.Unparse.Problem.Rule
-- Description : Unparser for rewrite rules
--
-- This module defines functions to unparse single TRS 'Rule's and blocks of rules
-- in COPS and ARI format.
module TRSConversion.Unparse.Problem.Rule
  ( -- * COPS
    unparseCopsRules,
    unparseCopsRule,

    -- * ARI
    unparseAriSystems,
    unparseAriRules,
    unparseAriRule,
  )
where

import TRSConversion.Problem.Common.Rule (Rule (..))
import TRSConversion.Unparse.Problem.Term (unparsePrefixTerm, unparseTerm)
import TRSConversion.Unparse.Utils (prettyBlock)
import Prettyprinter (Doc, Pretty, emptyDoc, indent, parens, pretty, vsep, (<+>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-- | Unparse a list of 'Rule's into the expected [COPS format](http://project-coco.uibk.ac.at/problems/trs.php)
-- separated by newlines. Uses 'unparseCopsRule' to parse individual rules.
unparseCopsRules :: (Pretty f, Pretty v) => [Rule f v] -> Doc ann
unparseCopsRules rs =
  if null rs
    then prettyBlock "RULES" emptyDoc
    else prettyBlock "RULES" $ vsep (emptyDoc : [indent 2 $ unparseCopsRule r | r <- rs] ++ [emptyDoc])

-- | Unparse a single COPS rule into format "lhs -> rhs" using function 'unparseTerm'
-- on each side of the rule.
--
-- >>> unparseCopsRule $ Rule {lhs=Fun "f" [Var "x", Fun "a" []], rhs=Var "x"}
-- f(x,a) -> x
unparseCopsRule :: (Pretty f, Pretty v) => Rule f v -> Doc ann
unparseCopsRule (Rule l r) = unparseTerm l <+> "->" <+> unparseTerm r

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
