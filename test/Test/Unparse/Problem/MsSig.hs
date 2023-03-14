-- |
-- Module      : Test.Unparse.Problem.MsSig
-- Description : Unparsing tests for MSTRS signatures
--
-- This module defines test cases for unparsing 'MsSig's to COPS and ARI format
module Test.Unparse.Problem.MsSig (unparseMsSigTests) where

import Data.Conversion.Problem.Mstrs.MsSig (MsSig (..))
import Data.Conversion.Unparse.Problem.MsSig (unparseAriMsSig, unparseCopsMsSig)
import Test.HUnit
import Test.Unparse.Utils (assertUnparse)

-- | Tests for unparsing 'MsSig's into COPS format and ARI format
unparseMsSigTests :: Test
unparseMsSigTests = TestLabel "Test.Unparse.Problem.MsSig" $ TestList [unparseCopsMsSigTests, unparseAriMsSigTests]

-- | Tests for converting some example 'MsSig's to COPS format using 'unparseCopsMsSig'
unparseCopsMsSigTests :: Test
unparseCopsMsSigTests =
  TestList
    [ TestLabel (label ++ " [COPS]") (TestCase tc)
      | (label, sig, expected, _) <- testSigs,
        let tc = assertUnparse sig (Right . unparseCopsMsSig) expected
    ]

-- | Tests for converting some example 'MsSig's to ARI format using 'unparseAriMsSig'
unparseAriMsSigTests :: Test
unparseAriMsSigTests =
  TestList
    [ TestLabel (label ++ " [ARI]") (TestCase tc)
      | (label, sig, _, expected) <- testSigs,
        let tc = assertUnparse sig (Right . unparseAriMsSig) expected
    ]

------------------------
--- Test data ----------
------------------------

-- | Example MSTRS signatures for testing along with expected output results.
-- Consists of tuples @(test label, sig, COPS format, ARI format)@
testSigs :: [(String, [MsSig String String], String, String)]
testSigs =
  [ ( "Test unparsing an empty MSTRS signature",
      [],
      "(SIG \n)",
      ""
    ),
    ( "Test unparsing a single MSTRS unary function",
      [MsSig "f" (["Nat"], "Nat")],
      "(SIG \n  (f Nat -> Nat)\n)",
      "(fun f :sort (Nat Nat))"
    ),
    ( "Test unparsing a more complicated signature",
      [MsSig "f" (["Nat"], "Nat"), MsSig "a" ([], "Nat"), MsSig "g" (["Nat", "List"], "List")],
      "(SIG \n  (f Nat -> Nat)\n  (a -> Nat)\n  (g Nat List -> List)\n)",
      "(fun f :sort (Nat Nat))\n(fun a :sort (Nat))\n(fun g :sort (Nat List List))"
    )
  ]
