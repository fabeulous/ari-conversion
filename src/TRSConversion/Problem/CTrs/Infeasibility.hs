module TRSConversion.Problem.CTrs.Infeasibility (
    Infeasibility (..),
    mapInfeasibility,
)
where

import TRSConversion.Problem.CTrs.CTrs (CTrs, Condition, mapCTrs, mapCondition)

data Infeasibility f v = Infeasibility
    { ctrs :: CTrs f v
    , isTrs :: Bool
    , query :: [Condition f v]
    }
    deriving (Eq, Show)

mapInfeasibility :: (f -> f') -> (v -> v') -> Infeasibility f v -> Infeasibility f' v'
mapInfeasibility f v inf =
    Infeasibility
        { ctrs = mapCTrs f v (ctrs inf)
        , query = mapCondition f v <$> query inf
        , isTrs = isTrs inf
        }
