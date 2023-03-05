module Data.Conversion.Problem.Trs.Trs
  ( Trs (..),
  )
where

import Data.Conversion.Problem.Rule (Rule)
import Data.Conversion.Problem.Trs.Sig (Sig)

-- | Datatype for a TRS (link to COCO type)
data Trs f v = Trs
  { -- | This is the documentation for the 'rules' constructor
    rules :: [Rule f v],
    -- | This is the documentation for the 'variables' constructor
    variables :: [v],
    -- | This is the documentation for the 'signature' constructor
    signature :: [Sig f],
    -- | This is the documentation for the 'comment' constructor
    comment :: Maybe String
  }
  deriving (Show, Eq)
