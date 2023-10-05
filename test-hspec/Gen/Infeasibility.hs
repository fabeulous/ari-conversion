-- |

module Gen.Infeasibility where
import TRSConversion.Problem.CTrs.Infeasibility (Infeasibility (..))
import Hedgehog
import TRSConversion.Problem.Trs.Sig (Sig)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Gen.CTrs (genCTrs, genCondition)
import Control.Monad (replicateM)


genInfeasibility :: [Sig f] -> Gen v -> Gen (Infeasibility f v)
genInfeasibility sig varGen = do
  system <- genCTrs sig varGen
  nConditions <- Gen.integral (Range.constantFrom 1 3 6)
  conds <- replicateM nConditions (genCondition sig varGen)
  pure $ Infeasibility { ctrs = system
                       , query = conds
                       }
