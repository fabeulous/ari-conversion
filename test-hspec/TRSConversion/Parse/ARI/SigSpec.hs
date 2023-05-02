module TRSConversion.Parse.ARI.SigSpec where

import Data.Text (pack)
import Gen.Sig (genSig)
import qualified Hedgehog as H
import TRSConversion.Parse.ARI.Sig (parseAriSig)
import qualified TRSConversion.Parse.ARI.Utils as ARI
import TRSConversion.Problem.Common.Term (Term, termFunArities)
import TRSConversion.Problem.Trs.Sig (Sig)
import TRSConversion.Unparse.Problem.TrsSig (unparseAriSigs)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (many, parse)

spec :: Spec
spec = do
  describe "properties" $
    it "roundtrip" $
      hedgehog $ do
        sig <- H.forAll genSig
        H.tripping
          sig
          (pack . show . unparseAriSigs)
          (parse (ARI.toParser (many parseAriSig)) "testinput")

sigOfTerm :: Eq f => Term f v -> [Sig f]
sigOfTerm t =
  case termFunArities t of
    Left _ -> error "should not happen"
    Right sigs -> sigs
