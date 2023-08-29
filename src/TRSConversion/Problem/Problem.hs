{- |
Module      : TRSConversion.Problem.Problem
Description : Type definition Problems
-}
module TRSConversion.Problem.Problem (
    FormatType (..),
    Problem (..),
    System (..),
) where

import TRSConversion.Problem.CTrs.CTrs (CTrs, CondType)
import TRSConversion.Problem.Common.MetaInfo (MetaInfo)
import TRSConversion.Problem.MsTrs.MsTrs (MsTrs)
import TRSConversion.Problem.Trs.Trs (Trs)
import TRSConversion.Problem.CSTrs.CSTrs (CSTrs)
import TRSConversion.Problem.CSCTrs.CSCTrs (CSCTrs)

data FormatType
  = TrsFormat Int
  | MSTrsFormat Int
  | CTrsFormat CondType Int
  | CSTrsFormat Int
  | CSCTrsFormat CondType Int
  | LCTrsFormat Int
  deriving (Eq, Show)

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
