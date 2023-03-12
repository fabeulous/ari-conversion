-- |
-- Module      : Test.Parse.Trs.Ari
-- Description : Parsing tests for ARI TRSs
--
-- This module defines test cases for the function 'parseAriTrs'.
-- It is non-exhaustive, but intended to highlight any obvious errors.
module Test.Parse.Trs.Ari (parseAriTests) where

import Data.Conversion.Parse.ParseTrs (parseAriTrs)
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)
import Test.TestData.Trs (ariTrss)

-- | Test cases for 'parseAriTrs' including cases which should be parseable and cases which should fail
parseAriTests :: Test
parseAriTests = TestLabel "parseAriTests" $ TestList [parseAriTrsTests, badAriTrsTests]

-- | Test cases for 'parseAriTrs' which should succeed and match the expected output
parseAriTrsTests :: Test
parseAriTrsTests =
  assertParseList
    "parseAriTrs should succeed"
    ([(trsStr, res) | (_, _, trsStr, res) <- ariTrss])
    parseAriTrs

-- | Malformatted examples for which it is asserted that 'parseAriTrs' should not succeed.
-- This list is non-exhaustive, but checks for some common problems.
badAriTrsTests :: Test
badAriTrsTests = assertFailParseList "parseAriTrs should fail" badTrss parseAriTrs
  where
    badTrss :: [String]
    badTrss =
      [ "(fun f 1) \n (rule (f x) x)", -- No format given
        "(format MSTRS)(fun f 1)(rule (f x) (x))", -- Wrong format
        "(format TRS)\n(fun f 1)\n (rule (f x) x)\n(fun g 1) ", -- fun block after rule block
        "(rule (f x) x)\n(format TRS)\n(fun f 1)", -- rule before format
        "(format TRS)\n(fun f 1)\n(rule (g x) x)", -- function symbol g not in signature
        "(VAR x)(RULES f(x)->x)", -- COPS format
        "(format TRS)\n(fun f 1)\n(rule (f x) x)(meta-info (comment \"Some comment\"))" -- meta-info at end
      ]
