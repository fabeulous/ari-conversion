{-# LANGUAGE DeriveFunctor #-}
-- |
-- Module      : TRSConversion.Problem.Trs.TrsSig
-- Description : General TRS signature type definition
--
-- This module contains the 'TrsSig' type definition. Signatures may be specified in different ways in different
-- formats (i.e. only variables, only function symbols, or both) so 'TrsSig' supports all three.
module TRSConversion.Problem.Trs.TrsSig
  ( -- * Untyped Signature datatype
    TrsSig (..),

    -- * Re-exports
    Sig (..),
  )
where

import TRSConversion.Problem.Trs.Sig (Sig (..))

-- | Datatype for the signature of a general first-order TRS.
-- Defined this way as the signature can be given in one of 3 ways in a TRS definition:
--
-- * Only variables (COPS format)
--
-- * Both variables and function symbols with arity (COPS extended format:
-- should specify all symbols in the system)
--
-- * Only function symbols with arity (ARI format)
newtype TrsSig f = FunSig [Sig f]
  deriving (Eq, Show, Functor)
