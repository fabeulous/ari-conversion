module TRSConversion.Problem.CSTrs.CSTrs (
    CSTrs (..),
    ReplacementMap,
    mapCSTrs,
) where

import Data.IntMap (IntMap)

import TRSConversion.Problem.Common.Rule (Rule, mapRule)
import TRSConversion.Problem.Trs.TrsSig (TrsSig)

type ReplacementMap f = [(f, [Int])]

data CSTrs f v = CSTrs
    { rules :: IntMap [Rule f v]
    -- ^ A list of the MSTRS rewrite rules
    , signature :: TrsSig f
    -- ^ The signature (function symbols and corresponding sorts) for the MSTRS
    , replacementMap :: ReplacementMap f
    -- ^ The replacementMap
    , numSystems :: Int
    -- ^ number of rewrite systems
    }
    deriving (Show, Eq)

mapCSTrs :: (f1 -> f2) -> (v1 -> v2) -> CSTrs f1 v1 -> CSTrs f2 v2
mapCSTrs f v cstrs = CSTrs {rules = map (mapRule f v) <$> rules cstrs
                           , signature = fmap f (signature cstrs)
                           , replacementMap = fmap (\(a,is) -> (f a, is)) (replacementMap cstrs)
                           , numSystems = numSystems cstrs
                           }
