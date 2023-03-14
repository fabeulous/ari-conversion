-- |
-- Module      : Test.Parse.Trs.Sig
-- Description : Parsing tests for TRS signatures
--
-- This module defines test cases for signature parsing functions and for checking signature consistency.
module Test.Parse.Trs.Sig (parseSigTests) where

import Data.Conversion.Parse.Problem.Sig (parseCopsSig, parseFsymArity)
import Data.Conversion.Problem.Trs.Sig (Sig (..), checkDistinctSig)
import Data.Either (isLeft, isRight)
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)

-- | Test cases for TRS signature parsing and checking
parseSigTests :: Test
parseSigTests = TestLabel "Test.Parse.Trs.Sig" $ TestList [parseCopsSigTests, badCopsSigTests, checkDistinctSigs, checkInconsistentSigs, parseFsymArityTests]

-- | Simple test cases for whick 'parseFsymArity' should succeed and match the expected output.
parseFsymArityTests :: Test
parseFsymArityTests = assertParseList "parseFsymArity should succeed" validFsymArities parseFsymArity
  where
    validFsymArities :: [(String, Sig String)]
    validFsymArities =
      [ ("f 1", Sig "f" 1),
        (" + 2", Sig "+" 2),
        ("fun   3 ", Sig "fun" 3)
      ]

-- | Test cases for which 'parseCopsSig' should succeed and match the expected output.
-- Expects signature strings in the COCO TRS (extended format)[http://project-coco.uibk.ac.at/problems/trs.php#extended].
parseCopsSigTests :: Test
parseCopsSigTests = assertParseList "parseCopsSig should succeed" validSigs parseCopsSig
  where
    validSigs :: [(String, [Sig String])]
    validSigs =
      [ ("(f 1)", [Sig "f" 1]),
        ("(f 1) ", [Sig "f" 1]),
        (" (f 1)", [Sig "f" 1]),
        ("  ( f 1  )", [Sig "f" 1]),
        ("(f 2) (a 0) (h 1)", [Sig "f" 2, Sig "a" 0, Sig "h" 1]),
        ("(f 2) (f 1)", [Sig "f" 2, Sig "f" 1]),
        ("(g 25) (g 25)", [Sig "g" 25, Sig "g" 25])
      ]

-- | Tests for which 'parseCopsSig' should fail due to invalid signature formats
badCopsSigTests :: Test
badCopsSigTests = assertFailParseList "parseCopsSig should fail" badSigs parseCopsSig
  where
    badSigs :: [String]
    badSigs =
      [ "f 1",
        "f 1 g 2",
        "(f 1 g 2)",
        "h -1",
        "h (-1)",
        "(g (1))",
        "(f 1 2)",
        "(f 1) (h 1",
        "(f 1 (h 1)"
      ]

-- | Tests for the function 'checkDistinctSig' (used in TRS parsing to find duplicate function symbols in signatures).
-- 'checkDistinctSig' should succeed for the tested examples.
checkDistinctSigs :: Test
checkDistinctSigs =
  TestList
    [ TestCase (assertBool (show sig ++ " is a valid signature. Got " ++ show res) (isRight res))
      | sig <- validSigs,
        let res = checkDistinctSig sig
    ]
  where
    validSigs :: [[Sig String]]
    validSigs =
      [ [Sig "f" 1],
        [Sig "f" 2, Sig "a" 0, Sig "h" 1],
        []
      ]

-- | Tests for the function 'checkDistinctSig' (used in TRS parsing to find duplicate function symbols in signatures).
-- 'checkDistinctSig' should fail (indicating that the given signature contains the same function symbol multiple times)
-- for the given examples.
checkInconsistentSigs :: Test
checkInconsistentSigs = TestList [TestCase $ assertBadSig sig (checkDistinctSig sig) | sig <- badSigs]
  where
    assertBadSig :: [Sig String] -> Either String [Sig String] -> Assertion
    assertBadSig expected res = assertBool (show expected ++ " is not a valid signature. Got: " ++ show res) (isLeft res)
    badSigs :: [[Sig String]]
    badSigs =
      [ [Sig "f" 1, Sig "f" 2],
        [Sig "f" 1, Sig "f" 1],
        [Sig "f" 1, Sig "a" 0, Sig "f" 1, Sig "f" 2]
      ]
