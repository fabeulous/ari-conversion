{- |
Module      : TRSConversion.Problem.Problem
Description : Type definition Problems
-}
module TRSConversion.Problem.Problem (
  FormatType (..),
  Problem (..),
  System(..),
  mapSystem,
)
where

import TRSConversion.Problem.CTrs.CTrs (CTrs, CondType, mapCTrs)
import TRSConversion.Problem.Common.MetaInfo (MetaInfo)
import TRSConversion.Problem.MsTrs.MsTrs (MsTrs, mapMsTrs)
import TRSConversion.Problem.Trs.Trs (Trs, mapTrs)
import TRSConversion.Problem.CSTrs.CSTrs (CSTrs, mapCSTrs)
import TRSConversion.Problem.CSCTrs.CSCTrs (CSCTrs, mapCSCTrs)
import TRSConversion.Problem.CTrs.Infeasibility (Infeasibility, mapInfeasibility)

data FormatType
  = TrsFormat Int
  | MSTrsFormat Int
  | CTrsFormat CondType Int
  | CSTrsFormat Int
  | CSCTrsFormat CondType Int
  | LCTrsFormat Int
  | InfeasibilityCTrsFormat CondType
  | InfeasibilityTrsFormat
  deriving (Eq, Show)

data System f v s
    = Trs (Trs f v)
    | MSTrs (MsTrs f v s)
    | LCTrs (MsTrs f v s)
    | CTrs (CTrs f v)
    | CSTrs (CSTrs f v)
    | CSCTrs (CSCTrs f v)
    | Infeasibility (Infeasibility f v)
    deriving (Eq, Show)


mapSystem :: (f -> f') -> (v -> v') -> (s -> s') -> System f v s -> System f' v' s'
mapSystem f v _ (Trs sys) = Trs $ mapTrs f v sys
mapSystem f v _ (CTrs sys) = CTrs $ mapCTrs f v sys
mapSystem f v _ (CSTrs sys) = CSTrs $ mapCSTrs f v sys
mapSystem f v _ (CSCTrs sys) = CSCTrs $ mapCSCTrs f v sys
mapSystem f v s (MSTrs sys) = MSTrs $ mapMsTrs f v s sys
mapSystem f v s (LCTrs sys) = LCTrs $ mapMsTrs f v s sys
mapSystem f v _ (Infeasibility sys) = Infeasibility $ mapInfeasibility f v sys

data Problem f v s = Problem
    { metaInfo :: MetaInfo
    , system :: System f v s
    }
    deriving (Eq, Show)
