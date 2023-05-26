module TRSConversion.Problem.CSCTrs.CSCTrs (
    CSCTrs (..),
) where

import TRSConversion.Problem.CSTrs.CSTrs (ReplacementMap)
import TRSConversion.Problem.CTrs.CTrs (CTrs)

data CSCTrs f v = CSCTrs
    { replacementMap :: ReplacementMap f
    , ctrs :: CTrs f v
    }
    deriving (Show, Eq)
