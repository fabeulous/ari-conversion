-- |
-- Module      : TRSConversion.Problem.Trs.Trs
-- Description : TRS type definition
--
-- This module contains the 'Trs' type definition for representing
-- first-order term rewriting systems.
module TRSConversion.Problem.Trs.Trs
  ( -- * TRS Datatype
    Trs (..),

    -- * Re-exports needed to construct a TRS
    Rule (..),
    Term (..),
    TrsSig (..),
    Sig (..),
  )
where

import Data.IntMap (IntMap)

import TRSConversion.Problem.Common.Rule (Rule (..))
import TRSConversion.Problem.Common.Term (Term (..))
import TRSConversion.Problem.Trs.TrsSig (Sig (..), TrsSig (..))


-- | Datatype for first-order term rewriting systems (TRSs).
-- Function symbols have type @f@ and variables have type @v@ in the system.
data Trs f v = Trs
  { -- | A list of the TRS rewrite rules
    rules :: IntMap [Rule f v],
    -- | The signature (function symbols and arities) of the TRS.
    -- It is possible to specify only variables, only function symbols, or both (see 'TrsSig') in order to support more TRS formats.
    signature :: TrsSig f v,
    -- | Number of rewrite systems
    numSystems :: Int
  }
  deriving (Show, Eq)
