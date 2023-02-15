module Data.Conversion.Problem.TRS (
  TRS(..)
) where

import Data.Conversion.Problem.Rule

-- | Datatype for a TRS (link to COCO type)
data TRS f v = TRS { 
                    -- | This is the documentation for the 'rules' constructor
                    rules      :: [Rule f v]
                    -- | This is the documentation for the 'variables' constructor
                   , variables  :: [v]
                   -- | This is the documentation for the 'signature' constructor
                   , signature  :: Maybe [(f, Int)]
                   -- | This is the documentation for the 'comment' constructor
                   , comment :: Maybe String
                } deriving (Show)
