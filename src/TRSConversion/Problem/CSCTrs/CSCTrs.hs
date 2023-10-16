module TRSConversion.Problem.CSCTrs.CSCTrs (
    CSCTrs (..),
    mapCSCTrs,
) where

import TRSConversion.Problem.CSTrs.CSTrs (ReplacementMap)
import TRSConversion.Problem.CTrs.CTrs (CTrs, mapCTrs)

data CSCTrs f v = CSCTrs
    { replacementMap :: ReplacementMap f
    , ctrs :: CTrs f v
    }
    deriving (Show, Eq)

mapCSCTrs :: (f -> f') -> (v -> v') -> CSCTrs f v -> CSCTrs f' v'
mapCSCTrs f v csctrs = CSCTrs { replacementMap = fmap (\(a,is) -> (f a, is)) (replacementMap csctrs)
                              , ctrs = mapCTrs f v (ctrs csctrs)
                              }
