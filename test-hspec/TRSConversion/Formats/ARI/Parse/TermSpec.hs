module TRSConversion.Formats.ARI.Parse.TermSpec where

import Data.Text (pack)
import Gen.Sig (genSig)
import Gen.Term (genTerm, genVars)
import qualified Hedgehog as H
import TRSConversion.Formats.ARI.Parse.Term (parsePrefixTerm)
import qualified TRSConversion.Formats.ARI.Parse.Utils as ARI
import TRSConversion.Problem.Common.Term (Term, termFunArities)
import TRSConversion.Problem.Trs.Sig (Sig)
import TRSConversion.Unparse.ARI.Problem.Term (unparsePrefixTerm)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (parse)
import qualified TRSConversion.Problem.Common.Term as Term
import TRSConversion.Parse.Utils (unToken, mkToken)

spec :: Spec
spec = do
  describe "properties" $
    it "roundtrip" $
      hedgehog $ do
        sig <- H.forAll genSig
        let sig' = fmap dummyToken <$> sig
        term <- H.forAll (genTerm sig genVars)
        H.tripping
          term
          (pack . show . unparsePrefixTerm)
          (parse (ARI.toParser (parseTerm sig')) "testinput")

parseTerm sig = Term.map unToken unToken <$> parsePrefixTerm sig

dummyToken s = mkToken s 0 (length s) (pack s)

sigOfTerm :: Eq f => Term f v -> [Sig f]
sigOfTerm t =
  case termFunArities t of
    Left _ -> error "should not happen"
    Right sigs -> sigs
