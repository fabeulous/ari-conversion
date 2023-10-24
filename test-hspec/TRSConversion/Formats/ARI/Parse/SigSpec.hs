module TRSConversion.Formats.ARI.Parse.SigSpec where

import Data.Text (pack)
import Gen.Sig (genSig)
import qualified Hedgehog as H
import TRSConversion.Formats.ARI.Parse.Sig (parseAriSig)
import qualified TRSConversion.Formats.ARI.Parse.Utils as ARI
import TRSConversion.Problem.Common.Term (Term, termFunArities)
import TRSConversion.Problem.Trs.Sig (Sig)
import TRSConversion.Unparse.ARI.Problem.TrsSig (unparseAriSigs)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (many, parse)
import TRSConversion.Parse.Utils (unToken)

spec :: Spec
spec = do
  describe "properties" $
    it "roundtrip" $
      hedgehog $ do
        sig <- H.forAll genSig
        H.tripping
          sig
          (pack . show . unparseAriSigs)
          (parse (ARI.toParser (many parseAriSigString)) "testinput")

parseAriSigString = fmap unToken <$> parseAriSig

sigOfTerm :: Eq f => Term f v -> [Sig f]
sigOfTerm t =
  case termFunArities t of
    Left _ -> error "should not happen"
    Right sigs -> sigs
