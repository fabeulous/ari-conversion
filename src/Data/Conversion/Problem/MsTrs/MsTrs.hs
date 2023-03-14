-- |
-- Module      : Data.Conversion.Problem.MsTrs.MsTrs
-- Description : MSTRS type definition
--
-- This module contains the 'MsTrs' type definition for representing many-sorted term rewriting systems (MSTRSs).
module Data.Conversion.Problem.MsTrs.MsTrs
  ( -- * MSTRS Datatype
    MsTrs (..),

    -- * Re-exports needed to construct an MSTRS
    Rule (..),
    Term (..),
    MsSig (..),
    MetaInfo (..),
  )
where

import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..))
import Data.Conversion.Problem.Common.Rule (Rule (..))
import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Conversion.Problem.MsTrs.MsSig (MsSig (..))

-- | Datatype for a many-sorted term rewriting system (MSTRS).
-- Function symbols have type @f@, variables have type @v@, and term sorts have type @s@ in the system.
--
-- Specifying a list of 'sorts' is deliberately left optional to support different formats.
-- The sorts can then be inferred from the signature when pretty-printing the MSTRS if needed (see 'inferSorts').
data MsTrs f v s = MsTrs
  { -- | A list of the MSTRS rewrite rules
    rules :: [Rule f v],
    -- | The signature (function symbols and corresponding sorts) for the MSTRS
    signature :: [MsSig f s],
    -- | A list of sorts (if given). Will be @Nothing@ for COPS format and @Just ss@ for ARI format.
    sorts :: Maybe [s],
    -- | Additional information about the MSTRS (e.g. the origin and general comments). See 'MetaInfo' definition for more details.
    metaInfo :: MetaInfo
  }
  deriving (Show, Eq)
