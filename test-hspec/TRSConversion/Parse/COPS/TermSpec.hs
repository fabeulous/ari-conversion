module TRSConversion.Parse.COPS.TermSpec where

import Data.Text (pack)
import Gen.Sig (genSig)
import Gen.Term (genTerm, genVars)
import qualified Hedgehog as H
import TRSConversion.Parse.COPS.Term (parseTermFuns)
import qualified TRSConversion.Parse.COPS.Utils as COPS
import TRSConversion.Problem.Common.Term (Term, termFunArities)
import TRSConversion.Problem.Trs.Sig (Sig (Sig))
import TRSConversion.Unparse.Problem.Term (unparseTerm)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (parse)
import Data.Containers.ListUtils (nubOrd)

spec :: Spec
spec = do
  describe "properties" $
    it "roundtrip" $
      hedgehog $ do
        sig <- H.forAll genSig
        term <- H.forAll (genTerm sig genVars)
        H.tripping
          term
          (pack . show . unparseTerm)
          (parse (COPS.toParser (parseTermFuns (nubOrd [f | Sig f _ <- sig]))) "testinput")

sigOfTerm :: Eq f => Term f v -> [Sig f]
sigOfTerm t =
  case termFunArities t of
    Left _ -> error "should not happen"
    Right sigs -> sigs
