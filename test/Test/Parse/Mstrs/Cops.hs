-- |
-- Module      : Test.Parse.Mstrs.Cops
-- Description : Parsing tests for COPS MSTRSs
--
-- This module defines test cases for the function 'parseCopsMstrs'.
-- It is non-exhaustive, but intended to highlight any obvious errors.
module Test.Parse.Mstrs.Cops (parseCopsMstrsTests) where

import Data.Conversion.Parse.ParseMstrs (parseCopsMstrs)
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)
import Test.TestData.Mstrs (copsMstrss)

-- | Test cases for 'parseCopsMstrs' including cases which should be parseable and cases which should fail
parseCopsMstrsTests :: Test
parseCopsMstrsTests = TestLabel "parseCopsMstrsTests" $ TestList [goodCopsMstrsTests, badCopsMstrsTests]

-- | Test cases for 'parseCopsMstrs' which should succeed and match the expected output
goodCopsMstrsTests :: Test
goodCopsMstrsTests =
  assertParseList
    "parseCopsMstrs should succeed"
    [(mstrsStr, res) | (_, _, mstrsStr, res) <- copsMstrss]
    parseCopsMstrs

-- | Malformatted examples for which it is asserted that 'parseCopsMstrs' should not succeed.
-- This list is non-exhaustive, but checks for some common problems.
badCopsMstrsTests :: Test
badCopsMstrsTests = assertFailParseList "parseCopsMstrs should fail" badTrss parseCopsMstrs
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
