module TRSConversion.Formats.COPS.Parse.TrsSpec where

import Data.Either (fromRight)
import Data.List (sort)
import Data.Text (pack)
import Gen.Sig (genSig)
import Gen.Term (genVars)
import Gen.Trs (genTrs)
import qualified Hedgehog as H
import TRSConversion.Formats.COPS.Parse.Trs (parseCopsTrs)
import qualified TRSConversion.Formats.COPS.Parse.Utils as COPS
import TRSConversion.Problem.Common.Term (Term, termFunArities)
import TRSConversion.Problem.Trs.Sig (Sig)
import TRSConversion.Unparse.UnparseTrs (unparseCopsTrs)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "properties" $
    it "roundtrip" $
    hedgehog $ do
      sig <- sort <$> H.forAll genSig
      trs <- H.forAll (genTrs sig genVars)
      H.tripping
        trs
        (pack . show . fromRight (error "invalid unparse") . unparseCopsTrs)
        (parse (COPS.toParser parseCopsTrs) "testinput")

sigOfTerm :: (Eq f) => Term f v -> [Sig f]
sigOfTerm t =
  case termFunArities t of
    Left _ -> error "should not happen"
    Right sigs -> sigs
