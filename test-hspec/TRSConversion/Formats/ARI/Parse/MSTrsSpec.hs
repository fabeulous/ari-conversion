module TRSConversion.Formats.ARI.Parse.MSTrsSpec where

import Data.Text (pack)
import Gen.MSTrs (genMsTrsString)
import Gen.MsSig (genMsSig)
import Gen.Term (genVars)
import qualified Hedgehog as H
import TRSConversion.Formats.ARI.Parse.MSTrs (parseAriMsTrs)
import qualified TRSConversion.Formats.ARI.Parse.Utils as ARI
import TRSConversion.Problem.Common.Term (Term, termFunArities)
import TRSConversion.Problem.Trs.Sig (Sig)
import TRSConversion.Unparse.ARI.UnparseMsTrs (unparseAriMsTrs)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (parse)
import TRSConversion.Problem.MsTrs.MsTrs (mapMsTrs)
import TRSConversion.Parse.Utils (unToken)

spec :: Spec
spec = do
  describe "properties" $
    it "roundtrip" $
      hedgehog $ do
        sig <- H.forAll genMsSig
        trs <- H.forAll (genMsTrsString sig genVars)
        H.tripping
          trs
          (pack . show . unparseAriMsTrs)
          (parse (ARI.toParser parseAriMsTrsString) "testinput")

parseAriMsTrsString = mapMsTrs unToken unToken unToken <$> parseAriMsTrs

sigOfTerm :: Eq f => Term f v -> [Sig f]
sigOfTerm t =
  case termFunArities t of
    Left _ -> error "should not happen"
    Right sigs -> sigs
