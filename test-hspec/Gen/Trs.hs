
module Gen.Trs where

import Hedgehog (Gen)
import TRSConversion.Problem.Trs.Sig (Sig (..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Control.Monad (replicateM)
import Gen.Rule (genRule)
import TRSConversion.Problem.Trs.Trs (Trs(..), TrsSig (FunSig))
import qualified Data.IntMap as IntMap


genTrs :: [Sig f] -> Gen v -> Gen (Trs f v)
genTrs sig varGen = do
  nRules <- Gen.integral (Range.constantFrom 1 3 6)
  rs <- replicateM nRules (genRule sig varGen)
  let trs = Trs { rules = IntMap.singleton 1 rs
                , signature = FunSig sig
                , numSystems = 1
                }
  pure trs
