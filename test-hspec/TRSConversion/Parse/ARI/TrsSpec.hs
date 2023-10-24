module TRSConversion.Formats.ARI.Parse.TrsSpec where

import Data.Either (fromRight)
import Data.Text (pack)
import Gen.Sig (genSig)
import Gen.Term (genVars)
import Gen.Trs (genTrs)
import qualified Hedgehog as H
import TRSConversion.Formats.ARI.Parse.Trs (parseAriTrs)
import qualified TRSConversion.Formats.ARI.Parse.Utils as ARI
import TRSConversion.Problem.Common.Term (Term, termFunArities)
import TRSConversion.Problem.Trs.Sig (Sig)
import TRSConversion.Unparse.UnparseTrs (unparseAriTrs)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (parse)
import TRSConversion.Problem.Trs.Trs (mapTrs)
import TRSConversion.Parse.Utils (unToken)

spec :: Spec
spec = do
  describe "properties" $
    it "roundtrip" $
      hedgehog $ do
        sig <- H.forAll genSig
        trs <- H.forAll (genTrs sig genVars)
        H.tripping
          trs
          (pack . show . fromRight (error "invalid unparse") . unparseAriTrs)
          (parse (ARI.toParser parseAriTrsString) "testinput")

parseAriTrsString = mapTrs unToken unToken <$> parseAriTrs

sigOfTerm :: Eq f => Term f v -> [Sig f]
sigOfTerm t =
  case termFunArities t of
    Left _ -> error "should not happen"
    Right sigs -> sigs
