-- |
-- Module      : Test.Unparse.UnparseTrs
-- Description : Unparsing tests for TRSs
--
-- This module defines test cases for converting first-order
-- TRSs from the internal 'Trs' representation to COPS and ARI format.
module Test.Unparse.UnparseTrs (unparseCopsTrsTests) where

import Data.Conversion.Parser.Unparse.UnparseTrs (unparseAri, unparseCops)
import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo)
import Data.Conversion.Problem.Common.Rule (Rule (..))
import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Data.Conversion.Problem.Trs.Trs (Trs (..))
import Data.Conversion.Problem.Trs.TrsSig (TrsSig (..))
import Prettyprinter (Pretty)
import Test.HUnit
import Test.Unparse.Utils (assertUnparseList)

-- | A TRS for testing with only variables specified with 'Vars'
trsVarSig :: Trs String String
trsVarSig =
  Trs
    { rules = [Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = Fun "g" [Fun "c" []]}],
      signature = Vars ["x", "y"],
      metaInfo = emptyMetaInfo
    }

-- | A TRS for testing with no 'rules'
trsNoRules :: Trs String String
trsNoRules =
  Trs
    { rules = [],
      signature = Vars ["x"],
      metaInfo = emptyMetaInfo
    }

-- | A TRS for testing with an empty signature
trsEmptySig :: Trs String String
trsEmptySig =
  Trs
    { rules = [Rule {lhs = Fun "a" [], rhs = Fun "b" []}],
      signature = Vars [],
      metaInfo = emptyMetaInfo {submitted = Just ["Person 1"]}
    }

-- | A TRS for testing with both variables and function symbols specified with 'FullSig'
trsFullSig :: Trs String String
trsFullSig =
  Trs
    { rules = [Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = Var "y"}],
      signature = FullSig ["x", "y"] [Sig "f" 2, Sig "a" 0, Sig "b" 1],
      metaInfo = emptyMetaInfo {comments = Just ["A TRS (with SIG given)"]}
    }

-- | A TRS for testing with just function symbols specified
trsFunSig :: Trs String String
trsFunSig =
  Trs
    { rules =
        [ Rule {lhs = Fun "nats" [], rhs = Fun ":" [Fun "0" [], Fun "inc" [Fun "nats" []]]},
          Rule {lhs = Fun "inc" [Fun ":" [Var "x", Var "y"]], rhs = Fun ":" [Fun "s" [Var "x"], Fun "inc" [Var "y"]]},
          Rule {lhs = Fun "tl" [Fun ":" [Var "x", Var "y"]], rhs = Var "y"},
          Rule {lhs = Fun "inc" [Fun "tl" [Fun "nats" []]], rhs = Fun "tl" [Fun "inc" [Fun "nats" []]]}
        ],
      signature = FunSig [Sig "0" 0, Sig "nats" 0, Sig "s" 1, Sig "tl" 1, Sig "inc" 1, Sig ":" 2],
      metaInfo =
        emptyMetaInfo
          { comments = Just ["[7] Example 2"],
            doi = Just "10.1007/11805618_6",
            origin = Just "COPS #20",
            submitted = Just ["Takahito Aoto", "Junichi Yoshida", "Yoshihito Toyama"]
          }
    }

-- | Unparser for COPS format
unparseCopsTrs :: (Eq v, Pretty f, Pretty v) => Trs f v -> String
unparseCopsTrs trs = show $ unparseCops trs

-- | Tests for converting some example 'Trs's to COPS format
unparseCopsTrsTests :: Test
unparseCopsTrsTests = assertUnparseList trss unparseCopsTrs
  where
    trss :: [(Trs String String, String)]
    trss =
      [ (trsVarSig, "(VAR x y)\n(RULES \n  f(x,y) -> g(c)\n)"),
        (trsNoRules, "(VAR x)"),
        (trsEmptySig, "(VAR )\n(RULES \n  a -> b\n)\n(COMMENT \nsubmitted by: Person 1)"),
        (trsFullSig, "(VAR x y)\n(SIG (f 2) (a 0) (b 1))\n(RULES \n  f(x,y) -> y\n)\n(COMMENT \nA TRS (with SIG given))"),
        ( trsFunSig,
          "(VAR x y)\n\
          \(SIG (0 0) (nats 0) (s 1) (tl 1) (inc 1) (: 2))\n\
          \(RULES \n\
          \  nats -> :(0,inc(nats))\n\
          \  inc(:(x,y)) -> :(s(x),inc(y))\n\
          \  tl(:(x,y)) -> y\n\
          \  inc(tl(nats)) -> tl(inc(nats))\n\
          \)\n\
          \(COMMENT \n\
          \doi:10.1007/11805618_6\n\
          \[7] Example 2\n\
          \origin: COPS #20\n\
          \submitted by: Takahito Aoto, Junichi Yoshida, Yoshihito Toyama)" -- qqjf does not exactly match COPS comment
        )
      ]
