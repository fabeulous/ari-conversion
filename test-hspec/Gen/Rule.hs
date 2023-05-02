
module Gen.Rule where

import Hedgehog (Gen)
import TRSConversion.Problem.Trs.Sig (Sig (..))
import Gen.Term (genTerm)
import TRSConversion.Problem.Common.Rule (Rule(..))


genRule :: [Sig f] -> Gen v -> Gen (Rule f v)
genRule sig varGen = Rule <$> gTerm <*> gTerm
  where
    gTerm = genTerm sig varGen
