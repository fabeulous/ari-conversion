module Gen.CTrs where

import Control.Monad (replicateM)
import Gen.Term (genTerm)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import TRSConversion.Problem.CTrs.CTrs (CRule (..), CTrs (..), CondType (..), Condition ((:==)))
import TRSConversion.Problem.Trs.Sig (Sig (..))
import TRSConversion.Problem.Trs.TrsSig (TrsSig (FunSig))
import qualified Data.IntMap as IntMap

genCRule :: [Sig f] -> Gen v -> Gen (CRule f v)
genCRule sig genVars = do
  l <- gTerm
  r <- gTerm
  nConds <- Gen.integral (Range.constantFrom 0 0 2)
  conds <- replicateM nConds (genCondition sig genVars)
  pure $ CRule l r conds
 where
  gTerm = genTerm sig genVars

genCondition :: [Sig f] -> Gen v -> Gen (Condition f v)
genCondition sig genVars = (:==) <$> genTerm sig genVars <*> genTerm sig genVars

genCTrs :: [Sig f] -> Gen v -> Gen (CTrs f v)
genCTrs sig varGen = do
  condType <- Gen.element [Oriented, SemiEquational, Join]
  nRules <- Gen.integral (Range.constantFrom 1 3 5)
  rs <- replicateM nRules (genCRule sig varGen)
  pure $
    CTrs
      { conditionType = condType
      , rules = IntMap.singleton 1 rs
      , signature = FunSig sig
      , numSystems = 1
      }
