module Data.Conversion.Problem.Term.Type
  ( -- * 'Term' datatype re-exported from "Data.Rewriting.Term.Type"

    -- | Has constructors 'Var' for variables and 'Fun' for function symbols.
    Term (..),
    foldTerm,
    termFunArities,
    checkConsistentSig,
  )
where

import Data.List (nub)
import Data.Rewriting.Term.Type (Term (..), fold)
import Prettyprinter

-- | Fold terms
--
-- qqjf
foldTerm :: (v -> a) -> (f -> [a] -> a) -> Term f v -> a
foldTerm = fold

-- | Return a list of function symbols and arities
-- Removes duplicates and asserts that each function symbol name appears at most once
-- >>> example (to add)
termFunArities :: (Eq f, Show f) => Term f v -> Either String [(f, Int)]
termFunArities t = checkConsistentSig $ nub arities
  where
    arities = foldTerm (const id) (\f xs -> ((f, length xs) :) . foldr (.) id xs) t []

-- | Check that a list of function symbols contains each function symbol at most once
checkConsistentSig :: (Eq f, Show f) => [(f, Int)] -> Either String [(f, Int)]
checkConsistentSig sig =
  if distinct (map fst sig)
    then Right sig
    else Left $ "A function symbol appears with multiple arities in signature " ++ show sig
  where
    distinct :: Eq a => [a] -> Bool
    distinct xs = nub xs == xs

-- | Make 'Term' an instance of 'Pretty'
instance (Pretty f, Pretty v) => Pretty (Term f v) where
  pretty (Var x) = pretty x
  pretty (Fun f ts) = pretty f <> args
    where
      args = encloseSep lparen rparen comma [pretty ti | ti <- ts]
