{-# LANGUAGE OverloadedStrings #-}

module Gen.Term where

import Control.Monad (replicateM)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import TRSConversion.Problem.Common.Term (Term (..))
import TRSConversion.Problem.Trs.Sig (Sig (..))

varsFrom :: [v] -> Gen v
varsFrom = Gen.element

funsFrom :: [(f, Int)] -> Gen (f, Int)
funsFrom = Gen.element

genVars :: Gen String
genVars = varsFrom ["x", "y", "z", "v", "w"]

genTerm :: [Sig f] -> Gen v -> Gen (Term f v)
genTerm sig varGen = go
  where
    go = Gen.recursive
           Gen.choice
           [Var <$> varGen]
           [fun]

    fun = do
        Sig f a <- Gen.element sig
        Fun f <$> replicateM a go
