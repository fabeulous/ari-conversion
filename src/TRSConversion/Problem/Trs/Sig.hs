{-# LANGUAGE DeriveFunctor #-}
-- |
-- Module      : TRSConversion.Problem.Trs.Sig
-- Description : TRS signature type definition
--
-- This module contains the 'Sig' type definition and helper functions for
-- the signature of first-order term rewriting systems. @Sig(..)@ is re-exported
-- from 'TrsSig', so only import this module if 'TrsSig' is not needed.
module TRSConversion.Problem.Trs.Sig
  ( -- * Untyped Signature datatype
    Sig (..),

    -- * Helper functions
    checkDistinctSig,
  )
where

import Data.List (nub)
import Prettyprinter (Pretty, pretty, (<+>))

-- | Datatype for the signature of a single function symbol.
--
-- For example, a function symbol @f@ of arity 2 can be written as @Sig "f" 2@.
data Sig f
  = Sig
      f
      -- ^ The function symbol
      Int
      -- ^ The arity of the function symbol (a non-negative integer)
  deriving (Eq, Ord, Show, Functor)

-- | Checks that each function symbol appears at most once in a list of 'Sig's and returns the
-- original signature wrapped in a @Right@ if this is the case.
-- If a function symbol appears twice in the input signature list, then an error message of type @Left String@ is returned.
--
-- >>> checkDistinctSig [Sig "f" 2, Sig "g" 1]
-- Right [Sig "f" 2,Sig "g" 1]
--
-- >>> checkDistinctSig [Sig "f" 2, Sig "g" 1, Sig "f" 1]
-- Left "A function symbol appears multiple times in signature...
checkDistinctSig :: (Eq f) => [Sig f] -> Either String [Sig f]
checkDistinctSig sig =
  if distinct $ foldr (\(Sig fsym _) -> (fsym :)) [] sig
    then Right sig
    else Left $ "A function symbol appears multiple times in signature"
  where
    distinct :: Eq a => [a] -> Bool
    distinct xs = nub xs == xs

-- | Make 'Sig' an instance of @Pretty@
instance (Pretty f) => Pretty (Sig f) where
  pretty (Sig fsym arity) = pretty fsym <+> pretty arity
