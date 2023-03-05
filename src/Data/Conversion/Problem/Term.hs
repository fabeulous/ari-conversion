module Data.Conversion.Problem.Term
  ( Term (..),
    termFunArities,
  )
where

import Data.Conversion.Problem.Trs.Sig (Sig (..), checkConsistentSig)
import Data.List (nub)
import Prettyprinter (Pretty, comma, encloseSep, lparen, pretty, rparen)

-- | The type for a term with function symbol type @f@ and variable type @v@.
-- e.g. @g(x)@ might correspond to @Fun "g" [Var "x"]@ if @f@ and @v@ are strings
data Term f v
  = -- | 'Fun' represents a function application to a list of arguments
    Fun f [Term f v]
  | -- | 'Var' us used for a single variable
    Var v
  deriving (Show, Eq, Ord)

-- | Return a list of function symbols and arities
-- Removes duplicates and asserts that each function symbol name appears at most once
-- >>> example (to add)
termFunArities :: (Eq f, Show f) => Term f v -> Either String [Sig f]
termFunArities t = checkConsistentSig $ nub arities
  where
    -- \| Recursively get arities of each function symbol in term. Logic copied from Haskell package 'Data.Rewriting.Term.Type'
    foldTerm :: (v -> a) -> (f -> [a] -> a) -> Term f v -> a
    foldTerm var _ (Var v) = var v
    foldTerm var fun (Fun f ts) = fun f (fmap (foldTerm var fun) ts)
    arities = foldTerm (const id) (\f xs -> (Sig f (length xs) :) . foldr (.) id xs) t []

-- | Make 'Term' an instance of 'Pretty'
instance (Pretty f, Pretty v) => Pretty (Term f v) where
  pretty (Var x) = pretty x
  pretty (Fun f ts) = pretty f <> args
    where
      args = encloseSep lparen rparen comma [pretty ti | ti <- ts]
