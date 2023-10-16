module TRSConversion.Parse.COPS.CTrsSpec where

import Data.Either (fromRight)
import Data.Text (pack)
import Gen.CTrs
import Gen.Sig
import Gen.Term
import qualified Hedgehog as H
import TRSConversion.Parse.COPS.CTrs (parseCopsCTrs)
import qualified TRSConversion.Parse.COPS.Utils as COPS
import TRSConversion.Unparse.CTrs (unparseCopsCTrs)
import Test.Hspec
import Test.Hspec.Hedgehog
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "properties" $
    it "roundtrip" $
      hedgehog $ do
        sig <- H.forAll genSig
        trs <- H.forAll (genCTrs sig genVars)
        H.tripping
          trs
          (pack . show . fromRight (error "invalid unparse") . unparseCopsCTrs)
          (parse (COPS.toParser parseCopsCTrs) "testinput")
