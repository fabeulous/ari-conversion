-- |
-- Module      : Test.Parse.Mstrs.Ari
-- Description : Parsing tests for ARI MSTRSs
--
-- This module defines test cases for the function 'parseAriMstrs'.
-- It is non-exhaustive, but intended to highlight any obvious errors.
module Test.Parse.Mstrs.Ari (parseAriMstrsTests) where

import Data.Conversion.Parse.ParseMstrs (parseAriMstrs)
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)
import Test.TestData.Mstrs (ariMstrss)

-- | Test cases for 'parseAriMstrs' including cases which should be parseable and cases which should fail
parseAriMstrsTests :: Test
parseAriMstrsTests = TestLabel "Test.Parse.Mstrs.Ari" $ TestList [goodAriMstrsTests, badAriMstrsTests]

-- | Test cases for 'parseAriMstrs' which should succeed and match the expected output
goodAriMstrsTests :: Test
goodAriMstrsTests =
  assertParseList
    "parseAriMstrs should succeed"
    ([(trsStr, res) | (_, _, trsStr, res) <- ariMstrss])
    parseAriMstrs

-- | Malformatted examples for which it is asserted that 'parseAriMstrs' should not succeed.
-- This list is non-exhaustive, but checks for some common problems.
badAriMstrsTests :: Test
badAriMstrsTests = assertFailParseList "parseAriMstrs should fail" badTrss parseAriMstrs
  where
    badTrss :: [String]
    badTrss =
      [ "(sort Nat)(fun a :sort (Nat))(fun b :sort (Nat))(rule a b)", -- No format
        "(format TRS)(sort Nat)(fun a :sort (Nat))(fun b :sort (Nat))(rule a b)", -- Wrong format
        "(format TRS)(fun a :sort (Nat))(fun b :sort (Nat))(sort Nat)(rule a b)" -- sort defined after signature
      ]
