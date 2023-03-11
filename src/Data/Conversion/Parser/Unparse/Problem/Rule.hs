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
-- separated by newlines. Uses 'unparseCopsRule' to parse individual rules.
--
-- If no rules are given, returns @Nothing@ and otherwise returns @Just rulesBlock@.
unparseCopsRules :: (Pretty f, Pretty v) => [Rule f v] -> Maybe (Doc ann)
unparseCopsRules rs =
  if null rs
    then Nothing
    else Just $ prettyBlock "RULES" $ vsep (emptyDoc : [indent 2 $ unparseCopsRule r | r <- rs] ++ [emptyDoc])

-- | Unparse a single COPS rule into format "lhs -> rhs" using function 'unparseTerm'
--
-- >>> unparseCopsRule $ Rule {lhs=Fun "f" [Var "x", Fun "a" []], rhs=Var "x"}
-- f(x,a) -> x
unparseCopsRule :: (Pretty f, Pretty v) => Rule f v -> Doc ann
unparseCopsRule (Rule l r) = unparseTerm l <+> pretty "->" <+> unparseTerm r

-- | Unparse a list of 'Rule's into [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/trs.txt)
-- separated by newlines. Uses 'unparseAriRule' to parse individual rules.
--
-- If no rules are given, returns @Nothing@ and otherwise returns @Just rulesBlock@.
unparseAriRules :: (Pretty f, Pretty v) => [Rule f v] -> Maybe (Doc ann)
unparseAriRules rs =
  if null rs
    then Nothing
    else Just $ vsep (map (\r -> parens $ pretty "rule" <+> unparseAriRule r) rs)

-- | Unparse a 'Rule' into the format expected by ARI. Uses 'parensTerm' to parse each side of the rule into prefix format.
--
-- >>> unparseCopsRule $ Rule {lhs=Fun "f" [Var "x", Fun "a" []], rhs=Var "x"}
-- (rule (f x a) x)
--
-- >>> unparseCopsRule $ Rule {lhs=Fun "f" [Var "x", Fun "g" [Var "y"]], rhs=Fun "g" [Var "y"]}
-- (rule (f x (g y)) (g y))
unparseAriRule :: (Pretty f, Pretty v) => Rule f v -> Doc ann
unparseAriRule (Rule l r) = parensTerm l <+> parensTerm r
  where
    -- Only add parentheses if not a constant or variable
    parensTerm :: (Pretty f, Pretty v) => Term f v -> Doc ann
    parensTerm v@(Var _) = unparsePrefixTerm v
    parensTerm t@(Fun _ []) = unparsePrefixTerm t -- Constant
    parensTerm t = parens $ unparsePrefixTerm t