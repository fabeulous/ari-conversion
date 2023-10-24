module TRSConversion.Formats.ARI.Parse.CSTrsSpec where

import Data.Either (fromRight)
import Data.Text (pack)
import Gen.CSTrs (genCSTrs)
import Gen.Sig (genSig)
import Gen.Term (genVars)
import qualified Hedgehog as H
import TRSConversion.Formats.ARI.Parse.CSTrs (parseAriCSTrs)
import qualified TRSConversion.Formats.ARI.Parse.Utils as ARI
import TRSConversion.Formats.ARI.Unparse.CSTrs (unparseAriCSTrs)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (parse)
import TRSConversion.Problem.CSTrs.CSTrs (mapCSTrs)
import TRSConversion.Parse.Utils (unToken)

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
          (parse (ARI.toParser parseAriCSTrsString) "testinput")

parseAriCSTrsString = mapCSTrs unToken unToken <$> parseAriCSTrs
