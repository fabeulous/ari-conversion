module TRSConversion.Problem.CTrs.Infeasibility where

import TRSConversion.Problem.CTrs.CTrs (CTrs, Condition, mapCondition, mapCTrs)

data Infeasibility f v = Infeasibility
    { ctrs :: CTrs f v
    , query :: [Condition f v]
    } deriving (Eq, Show)

mapInfeasibility :: (f -> f') -> (v -> v') -> Infeasibility f v -> Infeasibility f' v'
mapInfeasibility f v inf =
  Infeasibility { ctrs = mapCTrs f v (ctrs inf)
                , query = mapCondition f v <$> query inf
                }
