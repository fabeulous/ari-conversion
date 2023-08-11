module TRSConversion.Parse.COPS.RuleSpec where

import Data.Text (pack)
import Gen.Sig (genSig)
import Gen.Term (genTerm, genVars)
import qualified Hedgehog as H
import TRSConversion.Parse.COPS.Rule (parseCopsRule)
import qualified TRSConversion.Parse.COPS.Utils as COPS
import TRSConversion.Problem.Common.Rule (Rule (..))
import TRSConversion.Problem.Common.Term (Term, termFunArities)
import TRSConversion.Problem.Trs.Sig (Sig)
import TRSConversion.Unparse.Problem.Rule (unparseCopsRule)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (parse)
import qualified TRSConversion.Problem.Common.Term as T
import Data.Containers.ListUtils (nubOrd)

spec :: Spec
spec = do
  describe "properties" $
    it "roundtrip" $
      hedgehog $ do
        sig <- H.forAll genSig
        term1 <- H.forAll (genTerm sig genVars)
        term2 <- H.forAll (genTerm sig genVars)
        let rule = Rule term1 term2
        let vs = nubOrd $ T.vars term1 ++ T.vars term2
        H.tripping
          rule
          (pack . show . unparseCopsRule)
          (parse (COPS.toParser (parseCopsRule vs)) "testinput")

sigOfTerm :: Eq f => Term f v -> [Sig f]
sigOfTerm t =
  case termFunArities t of
    Left _ -> error "should not happen"
    Right sigs -> sigs
