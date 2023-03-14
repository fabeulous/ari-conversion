-- |
-- Module      : Data.Conversion.Problem.Common.Rule
-- Description : Rule type definition
--
-- This module contains the 'Rule' type definition and helper functions for Rules.
module Data.Conversion.Problem.Common.Rule
  ( -- * Types
    Rule (..),

    -- * Helper functions
    ruleFunArities,
    inferRulesSignature,
    ruleVars,
  )
where

import Data.Conversion.Problem.Common.Term (Term (..), termFunArities)
import Data.Conversion.Problem.Trs.Sig (Sig, checkConsistentSig)
import Data.List (nub)

-- | Datatype representing a rewrite rule @lhs->rhs@.
data Rule f v = Rule
  { -- | The left-hand side of the rule
    lhs :: Term f v,
    -- | The right-hand side of the rule
    rhs :: Term f v
  }
  deriving (Ord, Eq, Show)

-- | Returns a list of the function symbols appearing on both sides of a 'Rule' and their arities (number of arguments).
-- Removes duplicates and asserts that each function symbol name has at most one arity.
--
-- >>>  ruleFunArities $ Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = Var "x"})
-- Right [Sig "f" 2]
--
-- >>> ruleFunArities $ Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = Fun "f" [Var "x"]}
-- Left "A function symbol appears multiple times in signature ...
ruleFunArities :: (Eq f, Show f) => Rule f v -> Either String [Sig f]
ruleFunArities (Rule l r) = do
  lhsArities <- termFunArities l
  rhsArities <- termFunArities r
  checkConsistentSig $ nub (lhsArities ++ rhsArities)

-- | Infer a signature from a list of rules by applying 'ruleFunArities' to each rule and then
-- checking that the union of the inferred signatures contain consistent arities.
--
-- >>> inferRulesSignature [Rule {lhs = Fun "f" [Var "x"], rhs = Var "x"}, Rule {lhs = Fun "g" [Var "x"], rhs = Var "x"}]
-- Right [Sig "f" 1,Sig "g" 1]
--
-- >>> inferRulesSignature [Rule {lhs = Fun "a" [Var "x"], rhs = Var "x"}, Rule {lhs = Fun "a" [], rhs = Fun "b" []}]
-- Left "A function symbol appears multiple times in signature...
inferRulesSignature :: (Eq f, Show f) => [Rule f v] -> Either String [Sig f]
inferRulesSignature rs = do
  case mapM ruleFunArities rs of
    -- Each individual signature might be consistent but we need to check their union
    Right sigLists -> checkConsistentSig $ nub (concat sigLists)
    Left err -> Left err

-- | Extract a list of variables appearing in both sides of a list of rules.
-- Duplicates are removed with 'nub'. Not very efficient, but it works.
--
-- >>> ruleVars [Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = Var "x"}
-- ["x", "y"]
ruleVars :: Eq v => [Rule f v] -> [v]
ruleVars rs = nub $ concatMap (\(Rule l r) -> termVars l ++ termVars r) rs
  where
    termVars :: Term f v -> [v]
    termVars (Var x) = [x]
    termVars (Fun _ ts) = concatMap termVars ts