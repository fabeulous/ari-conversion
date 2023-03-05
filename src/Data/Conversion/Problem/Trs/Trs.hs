-- |
-- Module      : Data.Conversion.Problem.Trs.Trs
-- Description : Trs type definition
--
-- This module contains the 'Trs' type definition for first-order term rewriting systems.
module Data.Conversion.Problem.Trs.Trs
  ( Trs (..),
  )
where

import Data.Conversion.Problem.Common.Rule (Rule)
import Data.Conversion.Problem.Trs.Sig (Sig)

-- | Datatype for first-order term rewriting systems (TRSs). 
-- Function symbols have type @f@ and variables have type @v@ in the system.
data Trs f v = Trs
  { -- | A list of the TRS rewrite rules
    rules :: [Rule f v],
    -- | A list of variables. Symbols in 'variables' will be interpreted as variables during parsing.
    variables :: [v],
    -- | The signature (function symbols and arities) of the TRS. qqjf: complete or not?
    signature :: [Sig f],
    -- | 'comment' is set to @Nothing@ if no comment is provided, otherwise @Just str@ if a comment @str@ is provided for the TRS.
    comment :: Maybe String
  }
  deriving (Show, Eq)
