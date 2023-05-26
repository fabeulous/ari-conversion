module Gen.CSCTrs where

import Hedgehog (Gen)
import TRSConversion.Problem.Trs.Sig (Sig (..))
import Gen.CSTrs (genReplacementMap)
import Gen.CTrs (genCTrs)
import TRSConversion.Problem.CSCTrs.CSCTrs (CSCTrs(..))

genCSCTrs :: [Sig f] -> Gen v -> Gen (CSCTrs f v)
genCSCTrs sig varGen = do
  repMap <- genReplacementMap sig
  system <- genCTrs sig varGen
  pure $
    CSCTrs
      { replacementMap = repMap
      , ctrs = system
      }
