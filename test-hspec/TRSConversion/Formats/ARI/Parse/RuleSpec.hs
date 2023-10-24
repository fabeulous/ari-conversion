module TRSConversion.Formats.ARI.Parse.RuleSpec where

import Control.Monad ((<=<))
import Data.Text (pack)
import Gen.Sig (genSig)
import Gen.Term (genTerm, genVars)
import qualified Hedgehog as H
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (parse)

import TRSConversion.Formats.ARI.Parse.Rule (parseAriRule)
import qualified TRSConversion.Formats.ARI.Parse.Utils as ARI
import qualified TRSConversion.Problem.Common.Index as Idx
import TRSConversion.Problem.Common.Rule (Rule (..))
import TRSConversion.Problem.Common.Term (Term, termFunArities)
import TRSConversion.Problem.Trs.Sig (Sig)
import TRSConversion.Unparse.ARI.Problem.Rule (unparseAriRule)
import TRSConversion.Formats.ARI.Parse.TermSpec (dummyToken)
import TRSConversion.Problem.Common.Rule (mapRule)
import TRSConversion.Parse.Utils (unToken)

spec :: Spec
spec = do
  describe "properties" $
    it "roundtrip" $
      hedgehog $ do
        sig <- H.forAll genSig
        let sig' = fmap dummyToken <$> sig
        term1 <- H.forAll (genTerm sig genVars)
        term2 <- H.forAll (genTerm sig genVars)
        let index = 1
        let rule = (index, Rule term1 term2)
        H.tripping
          rule
          (pack . show . uncurry unparseAriRule)
          ((\(i, r) -> pure (Idx.index i, r)) <=< parse (ARI.toParser (parseAriRuleString sig')) "testinput")

parseAriRuleString sig = (\(i,r) -> (i, mapRule unToken unToken r)) <$> parseAriRule sig

sigOfTerm :: Eq f => Term f v -> [Sig f]
sigOfTerm t =
  case termFunArities t of
    Left _ -> error "should not happen"
    Right sigs -> sigs
