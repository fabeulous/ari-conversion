-- |
-- Module      : Data.Conversion.Problem.Trs.TrsSig
-- Description : General TRS signature type definition
--
-- This module contains the 'TrsSig' type definition. Signatures may be specified in different ways in different
-- formats (i.e. only variables, only function symbols, or both) so 'TrsSig' supports all three.
module Data.Conversion.Problem.Trs.TrsSig
  ( -- * Untyped Signature datatype
    TrsSig (..),
  )
where

import Data.Conversion.Problem.Trs.Sig (Sig)
import Prettyprinter (Pretty, pretty, (<+>))

-- | Datatype for the signature of a general first-order TRS.
-- Defined this way as the signature can be given in one of 3 ways in a TRS definition:
-- * Just variables (COPS format)
-- * Both variables and function symbols with arity (COPS extended format: exhaustive)
-- * Just function symbols with arity (ARI format)
data TrsSig f v
  = -- | All variables in the TRS
    Vars [v]
  | -- | All function symbols in the TRS
    FunSig [Sig f]
  | -- | All variables and function symbols in the TRS
    FullSig [v] [Sig f]
  deriving (Ord, Eq, Show)

-- | Make 'TrsSig' an instance of @Pretty@
instance (Pretty f, Pretty v) => Pretty (TrsSig f v) where
  pretty (Vars vs) = pretty "Variables:" <+> pretty vs
  pretty (FunSig fs) = pretty "Function signature:" <+> pretty fs
  pretty (FullSig vs fs) = pretty "Variables:" <+> pretty vs <+> pretty "and function signature:" <+> pretty fs
