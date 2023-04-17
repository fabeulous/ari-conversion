module TRSConversion.Problem.CTrs.CTrs (
    CTrs (..),
    CRule (..),
    Condition (..),
    CondType (..),
) where

import TRSConversion.Problem.Common.Term
import TRSConversion.Problem.Trs.TrsSig (TrsSig)

data CondType = Oriented | Join | SemiEquational
    deriving (Eq, Show)

data Condition f v = (Term f v) :== (Term f v)
    deriving (Eq, Show)

data CRule f v = CRule
    { lhs :: Term f v
    -- ^ The left-hand side of the rule
    , rhs :: Term f v
    -- ^ The right-hand side of the rule
    , conditions :: [Condition f v]
    -- ^ Conditions
    }
    deriving (Eq, Show)

data CTrs f v = CTrs
    { conditionType :: CondType
    , rules :: [CRule f v]
    -- ^ A list of the MSTRS rewrite rules
    , signature :: TrsSig f v
    -- ^ The signature (function symbols and corresponding sorts) for the MSTRS
    }
    deriving (Show, Eq)
