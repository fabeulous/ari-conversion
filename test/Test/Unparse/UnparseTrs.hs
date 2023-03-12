-- |
-- Module      : Test.Unparse.UnparseTrs
-- Description : Unparsing tests for TRSs
--
-- This module defines test cases for converting first-order
-- TRSs from the internal 'Trs' representation to COPS and ARI format.
module Test.Unparse.UnparseTrs (unparseCopsTrsTests, unparseAriTrsTests) where

import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo)
import Data.Conversion.Problem.Common.Rule (Rule (..))
import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Data.Conversion.Problem.Trs.Trs (Trs (..))
import Data.Conversion.Problem.Trs.TrsSig (TrsSig (..))
import Data.Conversion.Unparse.UnparseTrs (unparseAriTrs, unparseCopsTrs)
import Prettyprinter (Pretty)
import Test.HUnit
import Test.Unparse.Utils (assertUnparseList)

-- | Unparser for COPS format
copsTrsUnparser :: (Eq v, Pretty f, Pretty v) => Trs f v -> String
copsTrsUnparser trs = case unparseCopsTrs trs of
  Right unparsed -> show unparsed
  Left err -> show err -- qqjf Add error handling

-- | Unparser for ARI format
ariTrsUnparser :: (Eq f, Eq v, Pretty f, Pretty v, Show f) => Trs f v -> String
ariTrsUnparser trs = case unparseAriTrs trs of
  Right unparsed -> show unparsed
  Left err -> show err -- qqjf Add error handling

-- | Tests for converting some example 'Trs's to COPS format
unparseCopsTrsTests :: Test
unparseCopsTrsTests = assertUnparseList trss copsTrsUnparser
  where
    trss :: [(Trs String String, String, String)]
    trss =
      [ (trsVarSig, "(VAR x y)\n(RULES \n  f(x,y) -> g(c)\n)", "Unparse simple COPS TRS"),
        (trsNoRules, "(VAR x)", "Unparse a TRS with no rules"),
        (trsEmptySig, "(VAR )\n(RULES \n  a -> b\n)\n(COMMENT \nsubmitted by: Person 1)", "Unparse COPS TRS with a comment block"),
        (trsFunSig, "(VAR x y z)\n(SIG (f 2) (a 0) (b 1))\n(RULES \n  f(x,y) -> y\n  b(z) -> a\n)", "Unparse COPS TRS with only function symbols specified"),
        (trsFullSig, "(VAR x y)\n(SIG (f 2) (a 0) (b 1))\n(RULES \n  f(x,y) -> y\n)\n(COMMENT \nA TRS (with SIG given))", "Unparse COPS TRS in extended format"),
        ( fullExampleTrs,
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
          \submitted by: Takahito Aoto, Junichi Yoshida, Yoshihito Toyama)", -- qqjf does not exactly match COPS comment
          "Unparse COPS TRS with MetaInfo"
        )
      ]

-- | Tests for converting some example 'Trs's to ARI format
unparseAriTrsTests :: Test
unparseAriTrsTests = assertUnparseList trss ariTrsUnparser
  where
    trss :: [(Trs String String, String, String)]
    trss =
      [ (trsVarSig, "(format TRS)\n(fun f 2)\n(fun g 1)\n(fun c 0)\n(rule (f x y) (g c))", "Unparse ARI TRS with only variables specified"),
        (trsNoRules, "(format TRS)", "Unparse a TRS with no rules"),
        (trsEmptySig, "(meta-info (submitted \"Person 1\"))\n(format TRS)\n(fun a 0)\n(fun b 0)\n(rule a b)", "Unparse ground TRS with one submitter"),
        (trsFunSig, "(format TRS)\n(fun f 2)\n(fun a 0)\n(fun b 1)\n(rule (f x y) y)\n(rule (b z) a)", "Unparse ARI TRS with only function symbols specified"),
        (trsFullSig, "(meta-info (comment \"A TRS (with SIG given)\"))\n(format TRS)\n(fun f 2)\n(fun a 0)\n(fun b 1)\n(rule (f x y) y)", "Unparse ARI TRS with full signature specified"),
        ( fullExampleTrs,
          "(meta-info (origin \"COPS #20\"))\n\
          \(meta-info (doi \"10.1007/11805618_6\"))\n\
          \(meta-info (comment \"[7] Example 2\"))\n\
          \(meta-info (submitted \"Takahito Aoto\" \"Junichi Yoshida\" \"Yoshihito Toyama\"))\n\
          \(format TRS)\n\
          \(fun 0 0)\n\
          \(fun nats 0)\n\
          \(fun s 1)\n\
          \(fun tl 1)\n\
          \(fun inc 1)\n\
          \(fun : 2)\n\
          \(rule nats (: 0 (inc nats)))\n\
          \(rule (inc (: x y)) (: (s x) (inc y)))\n\
          \(rule (tl (: x y)) y)\n\
          \(rule (inc (tl nats)) (tl (inc nats)))",
          "Unparse an ARI TRS with MetaInfo"
        )
      ]

------------------------
--- Test data ----------
------------------------

-- | A TRS for testing with only variables specified in the signature with 'Vars'
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

-- | A TRS for testing with an empty 'Vars' signature
trsEmptySig :: Trs String String
trsEmptySig =
  Trs
    { rules = [Rule {lhs = Fun "a" [], rhs = Fun "b" []}],
      signature = Vars [],
      metaInfo = emptyMetaInfo {submitted = Just ["Person 1"]}
    }

-- | A TRS for testing with only function symbols specified with 'FunSig'
trsFunSig :: Trs String String
trsFunSig =
  Trs
    { rules = [Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = Var "y"}, Rule {lhs = Fun "b" [Var "z"], rhs = Fun "a" []}],
      signature = FunSig [Sig "f" 2, Sig "a" 0, Sig "b" 1],
      metaInfo = emptyMetaInfo
    }

-- | A TRS for testing with both variables and function symbols specified with 'FullSig'
trsFullSig :: Trs String String
trsFullSig =
  Trs
    { rules = [Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = Var "y"}],
      signature = FullSig ["x", "y"] [Sig "f" 2, Sig "a" 0, Sig "b" 1],
      metaInfo = emptyMetaInfo {comment = Just "A TRS (with SIG given)"}
    }

-- | A TRS for testing with just function symbols specified with 'FunSig'
fullExampleTrs :: Trs String String
fullExampleTrs =
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
          { comment = Just "[7] Example 2",
            doi = Just "10.1007/11805618_6",
            origin = Just "COPS #20",
            submitted = Just ["Takahito Aoto", "Junichi Yoshida", "Yoshihito Toyama"]
          }
    }
