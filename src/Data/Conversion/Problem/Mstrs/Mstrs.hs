-- |
-- Module      : Data.Conversion.Problem.Mstrs.Mstrs
-- Description : MSTRS type definition
--
-- This module contains the 'Mstrs' type definition for many-sorted term rewriting systems.
module Data.Conversion.Problem.Mstrs.Mstrs
  ( Mstrs (..),
  )
where

import Data.Conversion.Problem.Common.MetaInfo (MetaInfo)
import Data.Conversion.Problem.Common.Rule (Rule)
import Data.Conversion.Problem.Mstrs.MsSig (MsSig)

-- | Datatype for a many-sorted term rewriting system (MSTRS).
-- Function symbols have type @f@, variables have type @v@, and term sorts have type @s@ in the system.
--
-- Specifying a list of 'sorts' is left optional deliberately to support different formats.
-- The sorts can then be inferred from the signature when pretty-printing the MSTRS if needed (see 'inferSorts').
data Mstrs f v s = Mstrs
  { -- | A list of the MSTRS rewrite rules
    rules :: [Rule f v],
    -- | The signature (function symbols and sorts) for the MSTRS
    signature :: [MsSig f s],
    -- | A list of sorts (if given). Will be @Nothing@ for COPS format and @Just [sorts]@ for ARI format.
    sorts :: Maybe [s],
    -- | Additional information about the MSTRS (e.g. the origin and general comments). See 'MetaInfo' definition for more details.
    metaInfo :: MetaInfo
  }
  deriving (Show, Eq)
