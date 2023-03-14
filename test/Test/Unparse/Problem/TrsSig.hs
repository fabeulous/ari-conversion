-- |
-- Module      : Test.Unparse.Problem.TrsSig
-- Description : Unparsing tests for TRS signatures
--
-- This module defines test cases for unparsing 'TrsSig's to COPS and ARI format
module Test.Unparse.Problem.TrsSig (unparseSigTests) where

import Data.Conversion.Problem.Common.Rule (Rule (..))
import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Data.Conversion.Problem.Trs.TrsSig (TrsSig (..))
import Data.Conversion.Unparse.Problem.TrsSig (unparseAriTrsSig, unparseCopsTrsSig)
import Test.HUnit
import Test.Unparse.Utils (assertUnparse)

-- | Tests for unparsing 'TrsSig's into COPS format and ARI format
unparseSigTests :: Test
unparseSigTests = TestLabel "unparseSigTests" $ TestList [unparseCopsSigTests, unparseAriSigTests]

-- | Tests for converting some example 'TrsSig's to COPS format 'unparseCopsTrsSig'
unparseCopsSigTests :: Test
unparseCopsSigTests =
  TestList
    [ TestLabel (label ++ " [COPS]") (TestCase tc)
      | (label, sig, rules, expected, _) <- testSigs,
        let tc = assertUnparse sig (unparseCopsTrsSig rules) expected
    ]

-- | Tests for converting some example 'TrsSig's to ARI format using 'unparseAriTrsSig'
unparseAriSigTests :: Test
unparseAriSigTests =
  TestList
    [ TestLabel (label ++ " [ARI]") (TestCase tc)
      | (label, sig, rules, _, expected) <- testSigs,
        let tc = assertUnparse sig (unparseAriTrsSig rules) expected
    ]

------------------------
--- Test data ----------
------------------------

-- | Example rules for a ground TRS which the signature @a/0, b/0 can be deduced
groundRules :: [Rule String String]
groundRules = [Rule {lhs = Fun "a" [], rhs = Fun "b" []}]

-- | Example rules for a TRS with only one function symbol @f@ and one variable @x@
singleVarRules :: [Rule String String]
singleVarRules = [Rule {lhs = Fun "f" [Var "x"], rhs = Var "x"}]

-- | Example rules from which the signature @c/0, f/1, g/2@ can be deduced and
-- for which @x@ and @y@ are variables.
exampleRules :: [Rule String String]
exampleRules =
  [ Rule {lhs = Fun "f" [Var "x"], rhs = Var "x"},
    Rule {lhs = Fun "g" [Var "x", Var "y"], rhs = Fun "c" []}
  ]

-- | Example TRS signatures for testing along with expected output results.
-- Consists of tuples @(test label, sig, rules corresponding to sig, COPS format, ARI format)@
testSigs :: [(String, TrsSig String String, [Rule String String], String, String)]
testSigs =
  [ -- Ground system test
    ("Unparse ground variable signature", Vars [], groundRules, "", "(fun a 0)\n(fun b 0)"),
    ("Unparse ground function signature", FunSig [Sig "a" 0, Sig "b" 0], groundRules, "(VAR )\n(SIG (a 0) (b 0))", "(fun a 0)\n(fun b 0)"),
    ("Unparse ground full signature", FullSig [] [Sig "a" 0, Sig "b" 0], groundRules, "(VAR )\n(SIG (a 0) (b 0))", "(fun a 0)\n(fun b 0)"),
    -- Test for system with one variable and one function symbol
    ("Unparse a single-variable signature", Vars ["x"], singleVarRules, "(VAR x)", "(fun f 1)"),
    ("Unparse a single-function signature", FunSig [Sig "f" 1], singleVarRules, "(VAR x)\n(SIG (f 1))", "(fun f 1)"),
    ("Unparse a single-function signature", FullSig ["x"] [Sig "f" 1], singleVarRules, "(VAR x)\n(SIG (f 1))", "(fun f 1)"),
    -- Tests for a system with multiple variables and function symbols
    ( "Unparse multi-variable signature",
      Vars ["x", "y"],
      exampleRules,
      "(VAR x y)",
      "(fun f 1)\n(fun g 2)\n(fun c 0)"
    ),
    ( "Unparse multi-function signature",
      FunSig [Sig "c" 0, Sig "f" 1, Sig "g" 2],
      exampleRules,
      "(VAR x y)\n(SIG (c 0) (f 1) (g 2))",
      "(fun c 0)\n(fun f 1)\n(fun g 2)"
    ),
    ( "Unparse multi-function and multi-variable signature",
      FullSig ["x", "y"] [Sig "c" 0, Sig "f" 1, Sig "g" 2],
      exampleRules,
      "(VAR x y)\n(SIG (c 0) (f 1) (g 2))",
      "(fun c 0)\n(fun f 1)\n(fun g 2)"
    )
  ]
