module Data.Conversion.Problem.Trs.Sig
  ( -- * Untyped Signature datatype
    Sig (..),
    checkConsistentSig,
  )
where

import Data.List (nub)
import Prettyprinter (Pretty, pretty, (<+>))

-- | Datatype for an untyped function symbol signature
-- To do: make into an abstract datatype?
data Sig f = Sig f Int -- qqjf fsym, arity
  deriving (Ord, Eq, Show)

-- | Check that a list of function symbols contains each function symbol at most once
checkConsistentSig :: (Show f) => [Sig f] -> Either String [Sig f]
checkConsistentSig sig =
  if distinct (foldr (\(Sig _ arity) xs -> arity : xs) [] sig)
    then Right sig
    else Left $ "A function symbol appears with multiple arities in signature " ++ show sig
  where
    distinct :: Eq a => [a] -> Bool
    distinct xs = nub xs == xs

-- | Make 'Sig' an instance of 'Pretty'
instance (Pretty f) => Pretty (Sig f) where
  pretty (Sig fsym arity) = pretty fsym <+> pretty "/" <+> pretty arity