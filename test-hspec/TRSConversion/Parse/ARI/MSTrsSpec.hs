module TRSConversion.Parse.ARI.MSTrsSpec where

import Data.Text (pack)
import Gen.MSTrs (genMsTrsString)
import Gen.MsSig (genMsSig)
import Gen.Term (genVars)
import qualified Hedgehog as H
import TRSConversion.Parse.ARI.MSTrs (parseAriMsTrs)
import qualified TRSConversion.Parse.ARI.Utils as ARI
import TRSConversion.Problem.Common.Term (Term, termFunArities)
import TRSConversion.Problem.Trs.Sig (Sig)
import TRSConversion.Unparse.UnparseMsTrs (unparseAriMsTrs)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (parse)

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
          (parse (ARI.toParser parseAriMsTrs) "testinput")

sigOfTerm :: Eq f => Term f v -> [Sig f]
sigOfTerm t =
  case termFunArities t of
    Left _ -> error "should not happen"
    Right sigs -> sigs