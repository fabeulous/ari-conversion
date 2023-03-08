-- |
-- Module      : Data.Conversion.Problem.Trs.Sig
-- Description : Signature type definition
--
-- This module contains the 'Sig' type definition and helper functions for the signature of first-order term rewriting systems.
module Data.Conversion.Problem.Trs.Sig
  ( -- * Untyped Signature datatype
    Sig (..),

    -- * Helper functions
    checkConsistentSig,
  )
where

import Data.List (nub)
import Prettyprinter (Pretty, pretty, (<+>))

-- | Datatype for the signature of a single function symbol.
--
-- For example, a function symbol @f@ of arity 2 may be written as @Sig "f" 2@.
data Sig f
  = Sig
      f
      -- ^ The function symbol
      Int
      -- ^ The arity of the function symbol (a non-negative integer)
  deriving (Ord, Eq, Show)

-- | Checks that each function symbol appears at most once in a list of 'Sig's and returns the original signature wrapped in a @Right@ in this case.
-- If a function symbol appears twice in the input signature list, then an error message of type @Left String@ is returned.
--
-- >>> checkConsistentSig [Sig "f" 2, Sig "g" 1]
-- Right [Sig "f" 2,Sig "g" 1]
--
-- >>> checkConsistentSig [Sig "f" 2, Sig "g" 1, Sig "f" 1]
-- Left "A function symbol appears multiple times in signature...
checkConsistentSig :: (Show f, Eq f) => [Sig f] -> Either String [Sig f]
checkConsistentSig sig =
  if distinct $ foldr (\(Sig fsym _) -> (fsym :)) [] sig
    then Right sig
    else Left $ "A function symbol appears multiple times in signature " ++ show sig
  where
    distinct :: Eq a => [a] -> Bool
    distinct xs = nub xs == xs

-- | Make 'Sig' an instance of @Pretty@
instance (Pretty f) => Pretty (Sig f) where
  pretty (Sig fsym arity) =  pretty fsym <+> pretty arity