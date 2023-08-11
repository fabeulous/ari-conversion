{- |
Module      : TRSConversion.Problem.Problem
Description : Type definition Problems
-}
module TRSConversion.Problem.Problem (
    Problem (..),
    System (..),
) where

import TRSConversion.Problem.CTrs.CTrs (CTrs)
import TRSConversion.Problem.Common.MetaInfo (MetaInfo)
import TRSConversion.Problem.MsTrs.MsTrs (MsTrs)
import TRSConversion.Problem.Trs.Trs (Trs)
import TRSConversion.Problem.CSTrs.CSTrs ( CSTrs )
import TRSConversion.Problem.CSCTrs.CSCTrs (CSCTrs)

data System
    = Trs (Trs String String)
    | MSTrs (MsTrs String String String)
    | CTrs (CTrs String String)
    | CSTrs (CSTrs String String)
    | CSCTrs (CSCTrs String String)
    deriving (Eq, Show)

data Problem = Problem
    { metaInfo :: MetaInfo
    , system :: System
    }
    deriving (Eq, Show)
