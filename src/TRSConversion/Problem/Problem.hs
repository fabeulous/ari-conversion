{- |
Module      : TRSConversion.Problem.Problem
Description : Type definition Problems
-}
module TRSConversion.Problem.Problem (
    Problem (..),
    System (..),
) where

import TRSConversion.Problem.Common.MetaInfo (MetaInfo)
import TRSConversion.Problem.MsTrs.MsTrs (MsTrs)
import TRSConversion.Problem.Trs.Trs (Trs)

data System
    = Trs (Trs String String)
    | MSTrs (MsTrs String String String)
    deriving (Eq, Show)

data Problem = Problem
    { metaInfo :: MetaInfo
    , system :: System
    } deriving (Eq, Show)
