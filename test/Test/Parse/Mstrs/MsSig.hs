-- |
-- Module      : Test.Parse.Mstrs.MsSig
-- Description : Parsing tests for MSTRS signatures
--
-- This module defines test cases for signature parsing functions for MSTRSs.
module Test.Parse.Mstrs.MsSig (msSigTests) where

import Data.Conversion.Parser.Parse.Problem.MsSig (parseCopsMsSig)
import Data.Conversion.Problem.Mstrs.MsSig (MsSig (..))
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)

-- | Test cases for MSTRS signature parsing and checking
msSigTests :: Test
msSigTests = TestList [parseCopsMsSigTests, badCopsMsSigTests]

-- | Test cases for which 'parseCopsMsSig' should succeed and match the expected output.
-- Expects signature strings in the COCO (MSTRS format)[http://project-coco.uibk.ac.at/problems/mstrs.php].
parseCopsMsSigTests :: Test
parseCopsMsSigTests = assertParseList validSigs parseCopsMsSig
  where
    validSigs :: [(String, MsSig String String)]
    validSigs =
      [ ("f   List  -> List", MsSig "f" (["List"], "List")),
        ("  f   List  -> List    ", MsSig "f" (["List"], "List")),
        ("c -> Nat", MsSig "c" ([], "Nat")),
        ("g T1 T2 T3 -> T1", MsSig "g" (["T1", "T2", "T3"], "T1"))
      ]

-- | Example strings for which 'parseCopsMsSig' should fail due to invalid signature formats
badCopsMsSigTests :: Test
badCopsMsSigTests = assertFailParseList badSigs parseCopsMsSig
  where
    badSigs :: [String]
    badSigs =
      [ "f List->List", -- No spaces
        "c ->Nat", -- No spaces
        "(f 1) (h 1)", -- Unsorted format
        "  f  -> List  -> List", -- Can not parse to end
        "f List -> List List", -- Can not parse to end
        "f Li\\st -> List List", -- \\ not allowed
        "f List -> Tree(1)", -- ( ) not allowed
        "f List -> \"Tree\"", -- "s not allowed
        "f List -> Tree,Tree" -- , not allowed
      ]
