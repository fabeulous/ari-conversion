module TRSConversion.Formats.ARI.Parse.CSCTrsSpec where

import Data.Either (fromRight)
import Data.Text (pack)
import Gen.CSCTrs (genCSCTrs)
import Gen.Sig (genSig)
import Gen.Term (genVars)
import qualified Hedgehog as H
import TRSConversion.Formats.ARI.Parse.CSCTrs (parseAriCSCTrs)
import qualified TRSConversion.Formats.ARI.Parse.Utils as ARI
import TRSConversion.Unparse.CSCTrs (unparseAriCSCTrs)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (parse)
import TRSConversion.Parse.Utils (unToken)
import TRSConversion.Problem.CSCTrs.CSCTrs (mapCSCTrs)

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
          (parse (ARI.toParser parseAriCSCTrsString) "testinput")

parseAriCSCTrsString = mapCSCTrs unToken unToken <$> parseAriCSCTrs
