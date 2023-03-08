-- |
-- Module      : Data.Conversion.Parser.Unparse.Problem.Rule
-- Description : Unparser for TRS rules
--
-- This module defines functions to unparse TRS 'rules' in COPS and ARI format.
module Data.Conversion.Parser.Unparse.Problem.Rule
  ( unparseCopsRules,
    unparseAriRules,
    unparseAriRule,
    unparseCopsRule,
  )
where

import Data.Conversion.Parser.Unparse.Problem.Term (unparsePrefixTerm, unparseTerm)
import Data.Conversion.Parser.Unparse.Utils (prettyBlock)
import Data.Conversion.Problem.Common.Rule (Rule (..))
import Data.Conversion.Problem.Common.Term (Term (..))
import Prettyprinter (Doc, Pretty, emptyDoc, indent, parens, pretty, vsep, (<+>))

-- | Unparse a list of 'Rule's into the expected [COPS format](http://project-coco.uibk.ac.at/problems/trs.php)
-- separated by newlines.
--
-- If no rules are given, returns @Nothing@ and otherwise returns @Just rulesBlock@.
unparseCopsRules :: (Pretty f, Pretty v) => [Rule f v] -> Maybe (Doc ann)
unparseCopsRules rs =
  if null rs
    then Nothing
    else Just $ prettyBlock "RULES" $ vsep (emptyDoc : [indent 2 $ unparseCopsRule r | r <- rs] ++ [emptyDoc])

-- | qqjf
unparseCopsRule :: (Pretty f, Pretty v) => Rule f v -> Doc ann
unparseCopsRule (Rule l r) = unparseTerm l <+> pretty "->" <+> unparseTerm r

-- | qqjf
unparseAriRules :: (Pretty f, Pretty v) => [Rule f v] -> Maybe (Doc ann)
unparseAriRules = undefined

-- | qqjf
-- qqjf A single variable will not be wrapped in parentheses, but all other terms will be.
unparseAriRule :: (Pretty f, Pretty v) => Rule f v -> Doc ann
unparseAriRule (Rule l r) = parensTerm l <+> parensTerm r
  where
    -- Only add parentheses if not a constant or variable
    parensTerm :: (Pretty f, Pretty v) => Term f v -> Doc ann
    parensTerm v@(Var _) = unparsePrefixTerm v
    parensTerm t@(Fun _ []) = unparsePrefixTerm t -- Constant
    parensTerm t = parens $ unparsePrefixTerm t