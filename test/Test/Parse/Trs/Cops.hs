-- |
-- Module      : Test.Parse.Trs.Cops
-- Description : Parsing tests for COPS TRSs
--
-- This module defines test cases for the function 'parseCopsTrs'.
-- It is non-exhaustive, but intended to highlight any obvious errors.
module Test.Parse.Trs.Cops (parseCopsTests) where

import Data.Conversion.Parser.Parse.ParseTrs (parseCopsTrs)
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)
import Test.TestData.Trs (copsTrss)

-- | Test cases for 'parseCopsTrs' including cases which should be parseable and cases which should fail
parseCopsTests :: Test
parseCopsTests = TestLabel "parseCopsTests" $ TestList [parseCopsTrsTests, badCopsTrsTests]

-- | Test cases for 'parseCopsTrs' which should succeed and match the expected output
parseCopsTrsTests :: Test
parseCopsTrsTests =
  assertParseList
    "parseCopsTrs should succeed"
    ([(trsStr, res) | (_, _, trsStr, res) <- copsTrss])
    parseCopsTrs

-- | Malformatted examples for which it is asserted that 'parseCopsTrs' should not succeed.
-- This list is non-exhaustive, but checks for some common problems.
badCopsTrsTests :: Test
badCopsTrsTests = assertFailParseList "parseCopsTrs shoudl fail" badTrss parseCopsTrs
  where
    badTrss :: [String]
    badTrss =
      [ "(VAR x y) (RULES f(x)->y \n f(x,y)->y )", -- Mixed arities of f
        "(VAR x y) (RULES f(x,y)->f(x) \n f(x,y)->y )", -- Mixed arities of f
        "(RULES f(x)->y) (VAR x y) ", -- RULES before VARS
        "(VAR x)\n(SIG (f 1))(RULES f(x)->g(x))", -- SIG and VARS should contain all symbols
        "(SIG (f 1))(VAR x)(RULES f(x)->x)", -- SIG before VARS
        "(COMMENT (f 1))(VAR x)(RULES f(x)-x)", -- COMMENT at start
        "(VAR f x)(SIG (f 1 g 1))(RULES g(x)->x)", -- f in VAR and SIG
        "(format TRS)\n(fun f 1)\n(rule (f x) (x))" -- ARI format
      ]
