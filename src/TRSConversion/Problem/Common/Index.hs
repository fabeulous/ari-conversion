module TRSConversion.Problem.Common.Index (
    Index (..),
) where

data Index = Index
    { index :: Int
    , startOffset :: Int
    }
    deriving (Show)
