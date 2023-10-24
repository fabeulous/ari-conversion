module TRSConversion.Formats.ARI.Parse.CTrsSpec where

import Data.Either (fromRight)
import Data.Text (pack)
import Gen.CTrs (genCTrs)
import Gen.Sig (genSig)
import Gen.Term (genVars)
import qualified Hedgehog as H
import TRSConversion.Formats.ARI.Parse.CTrs (parseAriCTrs)
import qualified TRSConversion.Formats.ARI.Parse.Utils as ARI
import TRSConversion.Problem.Common.Term (Term, termFunArities)
import TRSConversion.Problem.Trs.Sig (Sig)
import TRSConversion.Unparse.CTrs (unparseAriCTrs)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (parse)
import TRSConversion.Parse.Utils ( unToken )
import TRSConversion.Problem.CTrs.CTrs (mapCTrs)

spec :: Spec
spec = do
  describe "properties" $
    it "roundtrip" $
      hedgehog $ do
        sig <- H.forAll genSig
        trs <- H.forAll (genCTrs sig genVars)
        H.tripping
          trs
          (pack . show . fromRight (error "invalid unparse") . unparseAriCTrs)
          (parse (ARI.toParser parseAriCTrsString) "testinput")

parseAriCTrsString = mapCTrs unToken unToken <$> parseAriCTrs

sigOfTerm :: Eq f => Term f v -> [Sig f]
sigOfTerm t =
  case termFunArities t of
    Left _ -> error "should not happen"
    Right sigs -> sigs
