-- |
-- Module      : Test.Unparse.UnparseMstrs
-- Description : Unparsing tests for MSTRSs
--
-- This module defines test cases for converting many-sorted TRSRs
-- from the internal 'Mstrs' representation to COPS and ARI format.
module Test.Unparse.UnparseMstrs (unparseCopsMstrsTests, unparseAriMstrsTests) where

import Data.Conversion.Unparse.UnparseMstrs (unparseAriMstrs, unparseCopsMstrs)
import Test.HUnit
import Test.TestData.Mstrs (ariMstrss, copsMstrss)
import Test.Unparse.Utils (assertUnparseList)

-- | Tests for converting some example 'Mstrs's to COPS format using 'unparseCopsMstrs'
unparseCopsMstrsTests :: Test
unparseCopsMstrsTests =
  assertUnparseList
    ([(res, mstrsStr, "unparseCopsMstrs should succeed on " ++ l) | (l, _, mstrsStr, res) <- copsMstrss])
    unparseCopsMstrs

-- | Tests for converting some example 'Mstrs's to ARI format using unparseAriMstrs''
unparseAriMstrsTests :: Test
unparseAriMstrsTests =
  assertUnparseList
    ([(trs, resStr, "unparseAriMstrs should succeed on " ++ l) | (l, trs, resStr, _) <- ariMstrss])
    (Right . unparseAriMstrs)