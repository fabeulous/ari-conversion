module TRSConversion.Parse.ARI.InfeasibilitySpec where

import Test.Hspec
import qualified Hedgehog as H
import qualified TRSConversion.Parse.ARI.Utils as ARI
import Test.Hspec.Hedgehog (hedgehog)
import Gen.Sig (genSig)
import Gen.Infeasibility (genInfeasibility)
import TRSConversion.Unparse.Infeasibility (unparseAriInfeasibility)
import TRSConversion.Parse.ARI.Infeasibility (parseAriInfeasibility)
import Gen.Term (genVars)
import Data.Text (pack)
import Data.Either (fromRight)
import Text.Megaparsec (parse)
import TRSConversion.Problem.CTrs.Infeasibility (mapInfeasibility)
import TRSConversion.Parse.Utils (unToken)

spec :: Spec
spec = do
  describe "properties" $
    it "roundtrip" $
      hedgehog $ do
        sig <- H.forAll genSig
        infSys <- H.forAll (genInfeasibility sig genVars)
        H.tripping
           infSys
           (pack . show . fromRight (error "invalid unparse") . unparseAriInfeasibility)
           (parse (ARI.toParser parseAriInfeasibilityString) "testinput")

parseAriInfeasibilityString = mapInfeasibility unToken unToken <$> parseAriInfeasibility
