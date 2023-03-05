module Data.Conversion.Problem.Term
  ( Term (..),
    foldTerm,
    termFunArities,
  )
where

import Data.Conversion.Problem.Trs.Sig (Sig (..), checkConsistentSig)
import Data.List (nub)
import Data.Rewriting.Term.Type (Term (..), fold)
import Prettyprinter (Pretty, comma, encloseSep, lparen, pretty, rparen)

-- | Fold terms
--
-- qqjf
foldTerm :: (v -> a) -> (f -> [a] -> a) -> Term f v -> a
foldTerm = fold

-- | Return a list of function symbols and arities
-- Removes duplicates and asserts that each function symbol name appears at most once
-- >>> example (to add)
termFunArities :: (Eq f, Show f) => Term f v -> Either String [Sig f]
termFunArities t = checkConsistentSig $ nub arities
  where
    arities = foldTerm (const id) (\f xs -> (Sig f (length xs) :) . foldr (.) id xs) t []

-- | Make 'Term' an instance of 'Pretty'
instance (Pretty f, Pretty v) => Pretty (Term f v) where
  pretty (Var x) = pretty x
  pretty (Fun f ts) = pretty f <> args
    where
      args = encloseSep lparen rparen comma [pretty ti | ti <- ts]
