module TRSConversion.Parse.COPS.TrsSpec where

import Data.Either (fromRight)
import Data.Text (pack)
import Gen.Sig (genSig)
import Gen.Term (genVars)
import Gen.Trs (genTrs)
import qualified Hedgehog as H
import TRSConversion.Parse.COPS.Trs (parseCopsTrs)
import qualified TRSConversion.Parse.COPS.Utils as COPS
import TRSConversion.Problem.Common.Term (Term, termFunArities)
import TRSConversion.Problem.Trs.Sig (Sig)
import TRSConversion.Unparse.UnparseTrs (unparseCopsTrs)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (parse)
import Data.Containers.ListUtils (nubOrd)
import TRSConversion.Problem.Trs.Trs (rules)
import Data.Foldable (toList)
import qualified TRSConversion.Problem.Common.Rule as R
import qualified TRSConversion.Problem.Common.Term as T
import TRSConversion.Problem.Trs.TrsSig (TrsSig(..))
import qualified TRSConversion.Problem.Trs.Trs as Trs

spec :: Spec
spec = do
  describe "properties" $
    it "roundtrip" $
      hedgehog $ do
        sig <- H.forAll genSig
        trs <- H.forAll (genTrs sig genVars)
        let vs = nubOrd $ concatMap (\(R.Rule lhs rhs) -> T.vars lhs ++ T.vars rhs) (concat $ toList (rules trs))
        let sig' = case Trs.signature trs of
                      (FunSig s) -> FullSig vs s
                      s@(FullSig _ _) -> s
                      Vars vs -> error "TODO: fix test TRSConversion.Parse.COPS.TrsSpec"
        let trs' = trs {Trs.signature = sig'}
        H.tripping
          trs'
          (pack . show . fromRight (error "invalid unparse") . unparseCopsTrs)
          (parse (COPS.toParser parseCopsTrs) "testinput")

sigOfTerm :: Eq f => Term f v -> [Sig f]
sigOfTerm t =
  case termFunArities t of
    Left _ -> error "should not happen"
    Right sigs -> sigs
