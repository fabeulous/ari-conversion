module TRSConversion.Parse.ARI.CSTrsSpec where

import Data.Either (fromRight)
import Data.Text (pack)
import Gen.CSTrs (genCSTrs)
import Gen.Sig (genSig)
import Gen.Term (genVars)
import qualified Hedgehog as H
import TRSConversion.Parse.ARI.CSTrs (parseAriCSTrs)
import qualified TRSConversion.Parse.ARI.Utils as ARI
import TRSConversion.Unparse.CSTrs (unparseAriCSTrs)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "properties" $
    it "roundtrip" $
      hedgehog $ do
        sig <- H.forAll genSig
        trs <- H.forAll (genCSTrs sig genVars)
        H.tripping
          trs
          (pack . show . fromRight (error "invalid unparse") . unparseAriCSTrs)
          (parse (ARI.toParser parseAriCSTrs) "testinput")
