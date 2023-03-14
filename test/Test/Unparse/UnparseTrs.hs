-- |
-- Module      : Test.Unparse.UnparseTrs
-- Description : Unparsing tests for TRSs
--
-- This module defines test cases for converting first-order
-- TRSs from the internal 'Trs' representation to COPS and ARI format.
module Test.Unparse.UnparseTrs (unparseTrsTests) where

import Data.Conversion.Unparse.UnparseTrs (unparseAriTrs, unparseCopsTrs)
import Test.HUnit
import Test.TestData.Trs (ariTrss, copsTrss)
import Test.Unparse.Utils (assertUnparseList)

-- | Tests for unparsing 'Trs's into COPS format and ARI format
unparseTrsTests :: Test
unparseTrsTests = TestLabel "Test.Unparse.UnparseTrs" $ TestList [unparseCopsTrsTests, unparseAriTrsTests]

-- | Tests for converting some example 'Trs's to COPS format using 'unparseCopsTrs'
unparseCopsTrsTests :: Test
unparseCopsTrsTests =
  assertUnparseList
    ([(res, trsStr, "unparseCopsTrs should succeed on " ++ l) | (l, _, trsStr, res) <- copsTrss])
    unparseCopsTrs

-- | Tests for converting some example 'Trs's to ARI format using 'unparseAriTrs'
unparseAriTrsTests :: Test
unparseAriTrsTests =
  assertUnparseList
    ([(trs, resStr, "unparseAriTrs should succeed on " ++ l) | (l, trs, resStr, _) <- ariTrss])
    unparseAriTrs
