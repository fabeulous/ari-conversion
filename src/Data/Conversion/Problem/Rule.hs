module Data.Conversion.Problem.Rule
  ( Rule (..),
    ruleFunArities,
  )
where

import Data.Conversion.Problem.Term (Term (..), termFunArities)
import Data.Conversion.Problem.Trs.Sig (Sig, checkConsistentSig)
import Data.List (nub)
import Prettyprinter (Pretty, hang, pretty, (<+>))

-- | Get function symbol arities from both sides of a rule and remove duplicates
ruleFunArities :: (Eq f, Show f) => Rule f v -> Either String [Sig f]
ruleFunArities (Rule l r) = do
  lhsArities <- termFunArities l
  rhsArities <- termFunArities r
  checkConsistentSig $ nub (lhsArities ++ rhsArities)

-- | Rewrite rule with left-hand side and right-hand side.
data Rule f v = Rule {lhs :: Term f v, rhs :: Term f v}
  deriving (Ord, Eq, Show)

-- | Make 'Rule' an instance of 'Pretty'
instance (Pretty f, Pretty v) => Pretty (Rule f v) where
  pretty (Rule l r) = hang 2 $ pretty l <+> pretty "->" <+> pretty r