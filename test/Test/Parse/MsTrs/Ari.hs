-- |
-- Module      : Test.Parse.MsTrs.Ari
-- Description : Parsing tests for ARI MSTRSs
--
-- This module defines test cases for the function 'parseAriMsTrs'.
-- It is non-exhaustive, but intended to highlight any obvious errors.
module Test.Parse.MsTrs.Ari (parseAriMsTrsTests) where

import Data.Conversion.Parse.ParseMsTrs (parseAriMsTrs)
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)
import Test.TestData.MsTrs (ariMsTrss)

-- | Test cases for 'parseAriMsTrs' including cases which should be parseable and cases which should fail
parseAriMsTrsTests :: Test
parseAriMsTrsTests = TestLabel "Test.Parse.MsTrs.Ari" $ TestList [goodAriMsTrsTests, badAriMsTrsTests]

-- | Test cases for 'parseAriMsTrs' which should succeed and match the expected output
goodAriMsTrsTests :: Test
goodAriMsTrsTests =
  assertParseList
    "parseAriMsTrs should succeed"
    ([(trsStr, res) | (_, _, trsStr, res) <- ariMsTrss])
    parseAriMsTrs

-- | Malformatted examples for which it is asserted that 'parseAriMsTrs' should not succeed.
-- This list is non-exhaustive, but checks for some common problems.
badAriMsTrsTests :: Test
badAriMsTrsTests = assertFailParseList "parseAriMsTrs should fail" badTrss parseAriMsTrs
  where
    badTrss :: [String]
    badTrss =
      [ "(sort Nat)(fun a :sort (Nat))(fun b :sort (Nat))(rule a b)", -- No format
        "(format TRS)(sort Nat)(fun a :sort (Nat))(fun b :sort (Nat))(rule a b)", -- Wrong format
        "(format TRS)(fun a :sort (Nat))(fun b :sort (Nat))(sort Nat)(rule a b)" -- sort defined after signature
      ]
