{-# LANGUAGE OverloadedStrings #-}

module Gen.Sig where

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import TRSConversion.Problem.Trs.Sig (Sig(..))

genSig :: Gen [Sig String]
genSig = do
    sigSize <- Gen.integral (Range.constantFrom 1 5 15)
    let fs = ["f" <> show i | i <- [1 .. sigSize :: Int]]
    mapM (\f -> Sig f <$> Gen.integral (Range.constantFrom 0 0 4)) fs
