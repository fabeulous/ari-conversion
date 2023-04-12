module Data.Conversion.Problem.CTrs.CTrs where

import Data.Conversion.Problem.Common.MetaInfo
import Data.Conversion.Problem.Common.Term
import Data.Conversion.Problem.Trs.Sig

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

data CTrs f v s = CTrs
    { rules :: [CRule f v]
    -- ^ A list of the MSTRS rewrite rules
    , signature :: [Sig f]
    -- ^ The signature (function symbols and corresponding sorts) for the MSTRS
    , metaInfo :: MetaInfo
    -- ^ Additional information about the MSTRS (e.g. the origin and general comments). See 'MetaInfo' definition for more details.
    }
    deriving (Show, Eq)
