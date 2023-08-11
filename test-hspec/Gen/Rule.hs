
module Gen.Rule where

import Hedgehog (Gen)
import TRSConversion.Problem.Trs.Sig (Sig (..))
import Gen.Term (genTerm)
import TRSConversion.Problem.Common.Rule (Rule(..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genIndexedRule :: [Sig f] -> Gen v -> Gen (Int, Rule f v)
genIndexedRule sig varGen =
  (,) <$> Gen.integral (Range.constantFrom 1 1 5) <*> genRule sig varGen

genRule :: [Sig f] -> Gen v -> Gen (Rule f v)
genRule sig varGen = Rule <$> gTerm <*> gTerm
  where
    gTerm = genTerm sig varGen
