module TRSConversion.Problem.CTrs.Infeasibility where

import TRSConversion.Problem.CTrs.CTrs (CTrs, Condition)

data Infeasibility f v = Infeasibility
    { ctrs :: CTrs f v
    , query :: [Condition f v]
    } deriving (Eq, Show)
