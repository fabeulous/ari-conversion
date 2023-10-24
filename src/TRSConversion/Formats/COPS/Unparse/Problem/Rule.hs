{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : TRSConversion.Formats.COPS.Unparse.Problem.Rule
-- Description : Unparser for rewrite rules
--
-- This module defines functions to unparse single TRS 'Rule's and blocks of rules
-- in COPS and ARI format.
module TRSConversion.Formats.COPS.Unparse.Problem.Rule
  ( -- * COPS
    unparseCopsRules,
    unparseCopsRule,
  )
where

import Prettyprinter (Doc, Pretty, emptyDoc, indent, vsep, (<+>))

import TRSConversion.Problem.Common.Rule (Rule (..))
import TRSConversion.Formats.COPS.Unparse.Problem.Term (unparseTerm)
import TRSConversion.Unparse.Utils (prettyBlock)


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

-- $setup
-- >>> import TRSConversion.Problem.Common.Term
