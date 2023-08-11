{- |
Module      : TRSConversion.Problem.MsTrs.MsTrs
Description : MSTRS type definition

This module contains the 'MsTrs' type definition for representing many-sorted term rewriting systems (MSTRSs).
-}
module TRSConversion.Problem.MsTrs.MsTrs (
  -- * MSTRS Datatype
  MsTrs (..),
  map,

  -- * Re-exports needed to construct an MSTRS
  Rule (..),
  Term (..),
  MsSig (..),
)
where

import Data.IntMap (IntMap)
import Prelude hiding (map)

import TRSConversion.Problem.Common.Rule (Rule (..))
import qualified TRSConversion.Problem.Common.Rule as Rule
import TRSConversion.Problem.Common.Term (Term (..))
import TRSConversion.Problem.MsTrs.MsSig (MsSig (..))
import qualified TRSConversion.Problem.MsTrs.MsSig as Sig

{- | Datatype for a many-sorted term rewriting system (MSTRS).
Function symbols have type @f@, variables have type @v@, and term sorts have type @s@ in the system.

Specifying a list of 'sorts' is deliberately left optional to support different formats.
The sorts can then be inferred from the signature when pretty-printing the MSTRS if needed (see 'inferSorts').
-}
data MsTrs f v s = MsTrs
  { rules :: IntMap [Rule f v]
  -- ^ A list of the MSTRS rewrite rules
  , signature :: [MsSig f s]
  -- ^ The signature (function symbols and corresponding sorts) for the MSTRS
  , sorts :: Maybe [s]
  -- ^ A list of sorts (if given). Will be @Nothing@ for COPS format and @Just ss@ for ARI format.
  , numSystems :: Int
  }
  deriving (Show, Eq)

map :: (f -> f') -> (v -> v') -> (s -> s') -> MsTrs f v s -> MsTrs f' v' s'
map f v s MsTrs{rules = rs, signature = sig, sorts = srts, numSystems = n} =
  MsTrs
    { rules = fmap (fmap $ Rule.map f v) rs
    , signature = fmap (Sig.map f s) sig
    , sorts = fmap (fmap s) srts
    , numSystems = n
    }
