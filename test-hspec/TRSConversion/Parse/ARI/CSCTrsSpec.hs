module TRSConversion.Parse.ARI.CSCTrsSpec where

import Data.Either (fromRight)
import Data.Text (pack)
import Gen.CSCTrs (genCSCTrs)
import Gen.Sig (genSig)
import Gen.Term (genVars)
import qualified Hedgehog as H
import TRSConversion.Parse.ARI.CSCTrs (parseAriCSCTrs)
import qualified TRSConversion.Parse.ARI.Utils as ARI
import TRSConversion.Unparse.CSCTrs (unparseAriCSCTrs)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "properties" $
    it "roundtrip" $
      hedgehog $ do
        sig <- H.forAll genSig
        trs <- H.forAll (genCSCTrs sig genVars)
        H.tripping
          trs
          (pack . show . fromRight (error "invalid unparse") . unparseAriCSCTrs)
          (parse (ARI.toParser parseAriCSCTrs) "testinput")
