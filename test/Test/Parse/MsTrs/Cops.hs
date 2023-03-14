-- |
-- Module      : Test.Parse.MsTrs.Cops
-- Description : Parsing tests for COPS MSTRSs
--
-- This module defines test cases for the function 'parseCopsMsTrs'.
-- It is non-exhaustive, but intended to highlight any obvious errors.
module Test.Parse.MsTrs.Cops (parseCopsMsTrsTests) where

import Data.Conversion.Parse.ParseMsTrs (parseCopsMsTrs)
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)
import Test.TestData.MsTrs (copsMsTrss)

-- | Test cases for 'parseCopsMsTrs' including cases which should be parseable and cases which should fail
parseCopsMsTrsTests :: Test
parseCopsMsTrsTests = TestLabel "Test.Parse.MsTrs.Cops" $ TestList [goodCopsMsTrsTests, badCopsMsTrsTests]

-- | Test cases for 'parseCopsMsTrs' which should succeed and match the expected output
goodCopsMsTrsTests :: Test
goodCopsMsTrsTests =
  assertParseList
    "parseCopsMsTrs should succeed"
    [(mstrsStr, res) | (_, _, mstrsStr, res) <- copsMsTrss]
    parseCopsMsTrs

-- | Malformatted examples for which it is asserted that 'parseCopsMsTrs' should not succeed.
-- This list is non-exhaustive, but checks for some common problems.
badCopsMsTrsTests :: Test
badCopsMsTrsTests = assertFailParseList "parseCopsMsTrs should fail" badTrss parseCopsMsTrs
  where
    badTrss :: [String]
    badTrss =
      [ "(RULES a->b)", -- No SIG block
        "(RULES f(x) -> x)(SIG (f Nat -> Nat))", -- SIG after rules
        "(COMMENT a comment)(SIG (f Nat -> Nat))(RULES f(x)->x)", -- COMMENT block first
        "(SIG (a -> Nat) (b -> Nat))(RULES a -> b)(RULES b -> a)", -- Two RULES blocks
        "(SIG (f Nat -> Nat))(RULES f(x) -> g(x))", -- Function symbol g not in SIG
        "(VAR x)(RULES f(x)->x)" -- COPS TRS format
      ]
