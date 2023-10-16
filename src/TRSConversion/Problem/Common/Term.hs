-- |
-- Module      : TRSConversion.Problem.Common.Term
-- Description : Term type and helpers
--
-- This module contains the 'Term' type definition and helper functions for Terms.
module TRSConversion.Problem.Common.Term
  ( -- * Types
    Term (..),
    map,
    vars,
    funs,
    -- * Helper Functions
    termFunArities,
  )
where

import Prelude hiding (map)
import TRSConversion.Problem.Trs.Sig (Sig (..), checkDistinctSig)
import Data.List (nub)

-- | The type for a term with function symbol type @f@ and variable type @v@.
--
-- For example, @g(x)@ might be represented as @Fun "g" [Var "x"]@ if @f@ and @v@ are of type @String@.
data Term f v
  = -- | 'Fun' represents a function application to a list of arguments
    Fun f [Term f v]
  | -- | 'Var' is used for a single variable
    Var v
  deriving (Show, Eq)

map :: (f -> f') -> (v -> v') -> Term f v -> Term f' v'
map _ v (Var x) = Var (v x)
map f v (Fun g ts) = Fun (f g) (fmap (map f v) ts)

-- | Returns a list of a function symbols appearing in a term and their arities (number of arguments).
-- Removes duplicates and asserts that each function symbol name has at most one arity.
--
-- >>> termFunArities $ Fun "f" [Var "x", Fun "g" [Var "y"]]
-- Right [Sig "f" 2,Sig "g" 1]
--
-- >>> termFunArities $ Fun "f" [Var "x", Fun "f" [Var "y"]]
-- Left "A function symbol appears multiple times in signature ...
--
-- >>> termFunArities $ Fun "f" [Var "x", Fun "f" [Var "y", Var "y"]]
-- Right [Sig "f" 2] -- Duplicates are removed
termFunArities :: (Eq f) => Term f v -> Either String [Sig f]
termFunArities t = checkDistinctSig $ nub arities
  where
    -- \| Recursively get arities of each function symbol in term. Logic copied from Haskell package 'Data.Rewriting.Term.Type'
    foldTerm :: (v -> a) -> (f -> [a] -> a) -> Term f v -> a
    foldTerm var _ (Var v) = var v
    foldTerm var fun (Fun f ts) = fun f (fmap (foldTerm var fun) ts)
    arities = foldTerm (const id) (\f xs -> (Sig f (length xs) :) . foldr (.) id xs) t []

vars :: Term f v -> [v]
vars (Var v) = [v]
vars (Fun _ ts) = concatMap vars ts

funs :: Term f v -> [f]
funs (Var _) = []
funs (Fun f ts) = f : concatMap funs ts


data Token a = Token { tokenValue :: a
                     , tokenOffset :: Int
                     , tokenLength :: Int
                     }
