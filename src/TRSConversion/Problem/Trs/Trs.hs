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
    mapTrs,
    emptyTrs,
  )
where

import Data.IntMap (IntMap, empty)

import TRSConversion.Problem.Common.Rule (Rule (..), mapRule)
import TRSConversion.Problem.Common.Term (Term (..))
import TRSConversion.Problem.Trs.TrsSig (Sig (..), TrsSig (..))


-- | Datatype for first-order term rewriting systems (TRSs).
-- Function symbols have type @f@ and variables have type @v@ in the system.
data Trs f v = Trs
  { -- | A list of the TRS rewrite rules
    rules :: IntMap [Rule f v],
    -- | The signature (function symbols and arities) of the TRS.
    -- It is possible to specify only variables, only function symbols, or both (see 'TrsSig') in order to support more TRS formats.
    signature :: TrsSig f,
    -- | Number of rewrite systems
    numSystems :: Int
  }
  deriving (Show, Eq)

mapTrs :: (f -> f') -> (v -> v') -> Trs f v -> Trs f' v'
mapTrs f v trs = Trs { rules = map (mapRule f v) <$> rules trs
                     , signature = fmap f (signature trs)
                     , numSystems = numSystems trs
                     }

emptyTrs :: Trs f v
emptyTrs = Trs { rules = Data.IntMap.empty
               , signature = FunSig []
               , numSystems = 1
               }