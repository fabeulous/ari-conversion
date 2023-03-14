-- |
-- Module      : Test.Unparse.UnparseMsTrs
-- Description : Unparsing tests for MSTRSs
--
-- This module defines test cases for converting many-sorted TRSRs
-- from the internal 'MsTrs' representation to COPS and ARI format.
module Test.Unparse.UnparseMsTrs (unparseMsTrsTests) where

import Data.Conversion.Unparse.UnparseMsTrs (unparseAriMsTrs, unparseCopsMsTrs)
import Test.HUnit
import Test.TestData.MsTrs (ariMsTrss, copsMsTrss)
import Test.Unparse.Utils (assertUnparseList)

-- | Tests for unparsing 'MsTrs's into COPS format and ARI format
unparseMsTrsTests :: Test
unparseMsTrsTests = TestLabel "Test.Unparse.UnparseMsTrs" $ TestList [unparseCopsMsTrsTests, unparseAriMsTrsTests]

-- | Tests for converting some example 'MsTrs's to COPS format using 'unparseCopsMsTrs'
unparseCopsMsTrsTests :: Test
unparseCopsMsTrsTests =
  assertUnparseList
    ([(res, mstrsStr, "unparseCopsMsTrs should succeed on " ++ l) | (l, _, mstrsStr, res) <- copsMsTrss])
    unparseCopsMsTrs

-- | Tests for converting some example 'MsTrs's to ARI format using unparseAriMsTrs''
unparseAriMsTrsTests :: Test
unparseAriMsTrsTests =
  assertUnparseList
    ([(trs, resStr, "unparseAriMsTrs should succeed on " ++ l) | (l, trs, resStr, _) <- ariMsTrss])
    (Right . unparseAriMsTrs)