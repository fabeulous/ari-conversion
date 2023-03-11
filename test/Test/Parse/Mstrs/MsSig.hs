-- |
-- Module      : Test.Parse.Mstrs.MsSig
-- Description : Parsing tests for MSTRS signatures
--
-- This module defines test cases for signature parsing functions for MSTRSs.
module Test.Parse.Mstrs.MsSig (msSigTests) where

import Data.Conversion.Parser.Parse.Problem.MsSig (parseAriMsSig, parseCopsMsSig)
import Data.Conversion.Problem.Mstrs.MsSig (MsSig (..))
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)

-- | Test cases for MSTRS signature parsing and checking
msSigTests :: Test
msSigTests = TestList [parseCopsMsSigTests, badCopsMsSigTests, parseAriMsSigTests, badAriMsSigTests]

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

-- | Test cases for which 'parseAriMsSig' should succeed and match the expected output.
-- Expects signature strings in the ARI (MSTRS format)[https://ari-informatik.uibk.ac.at/tasks/A/mstrs.txt].
parseAriMsSigTests :: Test
parseAriMsSigTests = assertParseList validSigs parseAriMsSig
  where
    validSigs :: [(String, MsSig String String)]
    validSigs =
      [ ("(fun s :sort (Nat Nat))", MsSig "s" (["Nat"], "Nat")),
        ("(fun 0 :sort (Nat))", MsSig "0" ([], "Nat")),
        (" ( fun  node   :sort   (Nat  Tree Tree Tree) ) ", MsSig "node" (["Nat", "Tree", "Tree"], "Tree")),
        ("(fun leaf :sort (Nat Tree))", MsSig "leaf" (["Nat"], "Tree")),
        ("(fun sum :sort (Tree Nat))", MsSig "sum" (["Tree"], "Nat"))
      ]

-- | Example strings for which 'parseAriMsSig' should fail due to invalid signature formats
badAriMsSigTests :: Test
badAriMsSigTests = assertFailParseList badSigs parseAriMsSig
  where
    badSigs :: [String]
    badSigs =
      [ "( s :sort (Nat Nat)))", -- No fun
        "(fun s (Nat Nat)))", -- No :sort
        "(fun s :sort ())", -- Empty sorts
        "(fun 0 :sort Nat)", -- No parentheses around Nat
        "(fun0 :sort Nat)", -- No space after fun
        "(fun s :sort (Nat Nat ))", -- Space after final sort
        "(fun s :sort(Nat Nat))", -- No space after :sort
        "(fun f() :sort (Nat Nat))", -- ( ) not allowed in function name
        "f List -> List", -- COPS format
        ""
      ]
