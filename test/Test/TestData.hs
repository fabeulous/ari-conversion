-- |
-- Module      : Test.TestData
-- Description : Example data for tests
--
-- This module defines test data which is used for testing both parsing and unparsing functions
-- to have a single source of truth. Exported values can tehn be imported in 'Test.Parse' and 'Test.Unparse'.
module Test.TestData
  ( -- * Test for tests on 'Trs'
    copsTrss,
    ariTrss,
  )
where

import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo)
import Data.Conversion.Problem.Common.Rule (Rule (..))
import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Data.Conversion.Problem.Trs.Trs (Trs (..))
import Data.Conversion.Problem.Trs.TrsSig (TrsSig (..))

------------------------
--- TRSs ----------
------------------------

-- | A list of TRSs in internal 'Trs' format and in (COPS format)[http://project-coco.uibk.ac.at/problems/trs.php]
-- to test both parsing and unparsing functions.
-- Has format @(Trs, COPS format, test label)@.
copsTrss :: [(Trs String String, String, String)]
copsTrss =
  [ (trsVarSig, "(VAR x y)\n(RULES \n  f(x,y) -> g(c)\n)", "simple COPS TRS"),
    (trsNoRules, "(VAR x)", "a TRS with no rules"),
    (trsEmptySig, "(VAR )\n(RULES \n  a -> b\n)\n(COMMENT \nsubmitted by: Person 1)", "COPS TRS with a comment block"),
    (trsFunSig, "(VAR x y z)\n(SIG (f 2) (a 0) (b 1))\n(RULES \n  f(x,y) -> y\n  b(z) -> a\n)", "COPS TRS with only function symbols specified"),
    (trsFullSig, "(VAR x y)\n(SIG (f 2) (a 0) (b 1))\n(RULES \n  f(x,y) -> y\n)\n(COMMENT \nA TRS (with SIG given))", "COPS TRS in extended format"),
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
      "COPS TRS with MetaInfo"
    )
  ]

-- | A list of TRSs in internal 'Trs' format and in (ARI format)[https://ari-informatik.uibk.ac.at/tasks/A/trs.txt]
-- to test both parsing and unparsing functions.
-- Has format @(Trs, COPS format, test label)@.
ariTrss :: [(Trs String String, String, String)]
ariTrss =
  [ (trsVarSig, "(format TRS)\n(fun f 2)\n(fun g 1)\n(fun c 0)\n(rule (f x y) (g c))", "ARI TRS with only variables specified"),
    (trsNoRules, "(format TRS)", "ARI TRS with no rules"),
    (trsEmptySig, "(meta-info (submitted \"Person 1\"))\n(format TRS)\n(fun a 0)\n(fun b 0)\n(rule a b)", "ground ARI TRS with one submitter"),
    (trsFunSig, "(format TRS)\n(fun f 2)\n(fun a 0)\n(fun b 1)\n(rule (f x y) y)\n(rule (b z) a)", "ARI TRS with only function symbols specified"),
    (trsFullSig, "(meta-info (comment \"A TRS (with SIG given)\"))\n(format TRS)\n(fun f 2)\n(fun a 0)\n(fun b 1)\n(rule (f x y) y)", "ARI TRS with full signature specified"),
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
      "ARI TRS with MetaInfo"
    )
  ]

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
      metaInfo = emptyMetaInfo {comments = Just ["A TRS (with SIG given)"]}
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
          { comments = Just ["[7] Example 2"],
            doi = Just "10.1007/11805618_6",
            origin = Just "COPS #20",
            submitted = Just ["Takahito Aoto", "Junichi Yoshida", "Yoshihito Toyama"]
          }
    }
