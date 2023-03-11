-- |
-- Module      : Data.Conversion.Problem.Trs.Trs
-- Description : TRS type definition
--
-- This module contains the 'Trs' type definition for first-order term rewriting systems.
module Data.Conversion.Problem.Trs.Trs
  ( Trs (..),
  )
where

import Data.Conversion.Problem.Common.MetaInfo (MetaInfo)
import Data.Conversion.Problem.Common.Rule (Rule)
import Data.Conversion.Problem.Trs.TrsSig (TrsSig)

-- | Datatype for first-order term rewriting systems (TRSs).
-- Function symbols have type @f@ and variables have type @v@ in the system.
data Trs f v = Trs
  { -- | A list of the TRS rewrite rules
    rules :: [Rule f v],
    -- | The signature (function symbols and arities) of the TRS. qqjf: complete or not?
    signature :: TrsSig f v,
    -- | Additional information about the TRS (e.g. the origin and general comments). See 'MetaInfo' definition for more details.
    metaInfo :: MetaInfo
  }
  deriving (Show, Eq)
