-- |
-- Module      : Test.TestData
-- Description : Example data for tests
--
-- This module defines test data which is used for testing both parsing and unparsing functions
-- to have a single source of truth. Exported values can tehn be imported in 'Test.Parse' and 'Test.Unparse'.
module Test.TestData
  ( -- * Test data for tests on 'Trs'
    copsTrss,
    ariTrss,

    -- * Example TRSs
  )
where

import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo)
import Data.Conversion.Problem.Common.Rule (Rule (..))
import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Data.Conversion.Problem.Trs.Trs (Trs (..))
import Data.Conversion.Problem.Trs.TrsSig (TrsSig (..))

------------------------
--- TRSs ---------------
------------------------

-- | A list of TRSs in internal 'Trs' format and in (COPS format)[http://project-coco.uibk.ac.at/problems/trs.php]
-- to test both parsing and unparsing functions.
-- Has format @(test label, original Trs, ARI string, Trs after unparsing)@.
--
-- The result for unparsing might differ from the original TRS if the original TRS signature only specified function symbols.
copsTrss :: [(String, Trs String String, String, Trs String String)]
copsTrss =
  [ ("simple COPS TRS", trsVarSig, "(VAR x y)\n(RULES \n  f(x,y) -> g(c)\n)", trsVarSig),
    ("COPS TRS with no rules", trsNoRules, "(VAR x)\n(RULES )", trsNoRules),
    ("COPS TRS with no rules or signature", minimalVarSigTrs, "(RULES )", minimalVarSigTrs),
    ( "COPS TRS with a comment block",
      groundTrs,
      "(RULES \n  a -> b\n)\n(COMMENT \nsubmitted by: Person 1)",
      Trs
        { rules = [ruleAToB],
          signature = Vars [],
          metaInfo = emptyMetaInfo {comments = Just ["submitted by: Person 1"]}
        }
    ),
    ( "COPS TRS with only function symbols specified",
      trsFunSig,
      "(VAR x y z)\n(SIG (f 2) (a 0) (b 1))\n(RULES \n  f(x,y) -> y\n  b(z) -> a\n)",
      Trs
        { rules = [ruleFxyToY, ruleBzToA],
          signature = FullSig ["x", "y", "z"] [Sig "f" 2, Sig "a" 0, Sig "b" 1],
          metaInfo = emptyMetaInfo
        }
    ),
    ( "COPS TRS in extended format",
      trsFullSig,
      "(VAR x y)\n(SIG (f 2) (a 0) (b 1))\n(RULES \n  f(x,y) -> y\n)\n(COMMENT \nA TRS (with SIG given))",
      trsFullSig
    ),
    ( "COPS TRS with MetaInfo",
      fullExampleTrs,
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
      Trs
        { rules = fullExampleRules,
          signature = FullSig ["x", "y"] [Sig "0" 0, Sig "nats" 0, Sig "s" 1, Sig "tl" 1, Sig "inc" 1, Sig ":" 2],
          metaInfo =
            emptyMetaInfo {comments = Just ["doi:10.1007/11805618_6\n[7] Example 2\norigin: COPS #20\nsubmitted by: Takahito Aoto, Junichi Yoshida, Yoshihito Toyama"]}
        }
    )
  ]

-- | A list of TRSs in internal 'Trs' format and in (ARI format)[https://ari-informatik.uibk.ac.at/tasks/A/trs.txt]
-- to test both parsing and unparsing functions.
-- Has format @(test label, original Trs, ARI string, Trs after unparsing)@.
--
-- The result for unparsing might differ from the original TRS as ARI TRSs only specify function symbols.
ariTrss :: [(String, Trs String String, String, Trs String String)]
ariTrss =
  [ ( "ARI TRS with only variables specified",
      trsVarSig,
      "(format TRS)\n(fun f 2)\n(fun g 1)\n(fun c 0)\n(rule (f x y) (g c))",
      Trs
        { rules = [ruleFxyToGc],
          signature = FunSig [Sig "f" 2, Sig "g" 1, Sig "c" 0],
          metaInfo = emptyMetaInfo
        }
    ),
    ( "ARI TRS with no rules",
      trsNoRules,
      "(format TRS)",
      Trs {rules = [], signature = FunSig [], metaInfo = emptyMetaInfo}
    ),
    ( "ARI TRS with no rules or signature",
      minimalVarSigTrs,
      "(format TRS)",
      Trs {rules = [], signature = FunSig [], metaInfo = emptyMetaInfo}
    ),
    ( "ground ARI TRS with one submitter",
      groundTrs,
      "(meta-info (submitted \"Person 1\"))\n(format TRS)\n(fun a 0)\n(fun b 0)\n(rule a b)",
      Trs
        { rules = [ruleAToB],
          signature = FunSig [Sig "a" 0, Sig "b" 0],
          metaInfo = emptyMetaInfo {submitted = Just ["Person 1"]}
        }
    ),
    ( "ARI TRS with only function symbols specified",
      trsFunSig,
      "(format TRS)\n(fun f 2)\n(fun a 0)\n(fun b 1)\n(rule (f x y) y)\n(rule (b z) a)",
      trsFunSig
    ),
    ( "ARI TRS with full signature specified",
      trsFullSig,
      "(meta-info (comment \"A TRS (with SIG given)\"))\n(format TRS)\n(fun f 2)\n(fun a 0)\n(fun b 1)\n(rule (f x y) y)",
      Trs
        { rules = [ruleFxyToY],
          signature = FunSig [Sig "f" 2, Sig "a" 0, Sig "b" 1],
          metaInfo = emptyMetaInfo {comments = Just ["A TRS (with SIG given)"]}
        }
    ),
    ( "ARI TRS with MetaInfo",
      fullExampleTrs,
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
      fullExampleTrs
    )
  ]

------------------------
--- Rules --------------
------------------------
ruleFxyToGc, ruleAToB, ruleFxyToY, ruleBzToA :: Rule String String
ruleFxyToGc = Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = Fun "g" [Fun "c" []]}
ruleAToB = Rule {lhs = Fun "a" [], rhs = Fun "b" []}
ruleFxyToY = Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = Var "y"}
ruleBzToA = Rule {lhs = Fun "b" [Var "z"], rhs = Fun "a" []}

-- | A TRS for testing with only variables specified in the signature with 'Vars'
trsVarSig :: Trs String String
trsVarSig =
  Trs
    { rules = [ruleFxyToGc],
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

-- | Minimal TRS
minimalVarSigTrs :: Trs String String
minimalVarSigTrs =
  Trs
    { rules = [],
      signature = Vars [],
      metaInfo = emptyMetaInfo
    }

-- | A TRS for testing with an empty 'Vars' signature
groundTrs :: Trs String String
groundTrs =
  Trs
    { rules = [ruleAToB],
      signature = Vars [],
      metaInfo = emptyMetaInfo {submitted = Just ["Person 1"]}
    }

-- | A TRS for testing with only function symbols specified with 'FunSig'
trsFunSig :: Trs String String
trsFunSig =
  Trs
    { rules = [ruleFxyToY, ruleBzToA],
      signature = FunSig [Sig "f" 2, Sig "a" 0, Sig "b" 1],
      metaInfo = emptyMetaInfo
    }

-- | A TRS for testing with both variables and function symbols specified with 'FullSig'
trsFullSig :: Trs String String
trsFullSig =
  Trs
    { rules = [ruleFxyToY],
      signature = FullSig ["x", "y"] [Sig "f" 2, Sig "a" 0, Sig "b" 1],
      metaInfo = emptyMetaInfo {comments = Just ["A TRS (with SIG given)"]}
    }

-- | Rules list based on COPS problem 20
fullExampleRules :: [Rule String String]
fullExampleRules =
  [ Rule {lhs = Fun "nats" [], rhs = Fun ":" [Fun "0" [], Fun "inc" [Fun "nats" []]]},
    Rule {lhs = Fun "inc" [Fun ":" [Var "x", Var "y"]], rhs = Fun ":" [Fun "s" [Var "x"], Fun "inc" [Var "y"]]},
    Rule {lhs = Fun "tl" [Fun ":" [Var "x", Var "y"]], rhs = Var "y"},
    Rule {lhs = Fun "inc" [Fun "tl" [Fun "nats" []]], rhs = Fun "tl" [Fun "inc" [Fun "nats" []]]}
  ]

-- | A TRS for testing with just function symbols specified with 'FunSig'
fullExampleTrs :: Trs String String
fullExampleTrs =
  Trs
    { rules = fullExampleRules,
      signature = FunSig [Sig "0" 0, Sig "nats" 0, Sig "s" 1, Sig "tl" 1, Sig "inc" 1, Sig ":" 2],
      metaInfo =
        emptyMetaInfo
          { comments = Just ["[7] Example 2"],
            doi = Just "10.1007/11805618_6",
            origin = Just "COPS #20",
            submitted = Just ["Takahito Aoto", "Junichi Yoshida", "Yoshihito Toyama"]
          }
    }
