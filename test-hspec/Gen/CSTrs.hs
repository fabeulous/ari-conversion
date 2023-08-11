{-# LANGUAGE TupleSections #-}

module Gen.CSTrs where

import Control.Monad (replicateM)
import Gen.Rule (genRule)
import Hedgehog (Gen)
import Hedgehog.Gen (subsequence)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import TRSConversion.Problem.CSTrs.CSTrs (CSTrs (..), ReplacementMap)
import TRSConversion.Problem.Trs.Sig (Sig (..))
import TRSConversion.Problem.Trs.TrsSig (TrsSig (..))
import qualified Data.IntMap as IntMap

genReplacementMap :: [Sig f] -> Gen (ReplacementMap f)
genReplacementMap = mapM (\(Sig f a) -> (f,) <$> genSubseq a)
 where
  genSubseq n = subsequence [1 .. n]

genCSTrs :: [Sig f] -> Gen v -> Gen (CSTrs f v)
genCSTrs sig varGen = do
  repMap <- genReplacementMap sig
  nRules <- Gen.integral (Range.constantFrom 1 3 5)
  rs <- replicateM nRules (genRule sig varGen)
  let trs =
        CSTrs
          { replacementMap = repMap
          , rules = IntMap.singleton 1 rs
          , signature = FunSig sig
          , numSystems = 1
          }
  pure trs
