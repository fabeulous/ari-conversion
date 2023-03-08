-- |
-- Module      : Test.Unparse.Problem.MetaInfo
-- Description : Unparsing tests for TRS MetaInfo
--
-- This module defines test cases for unparsing 'MetaInfo's to COPS and ARI format
module Test.Unparse.Problem.MetaInfo (unparseMetaInfoTests) where

import Data.Conversion.Parser.Unparse.Problem.MetaInfo (unparseAriMetaInfo, unparseCopsMetaInfo)
import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo)
import Test.HUnit
import Test.Unparse.Utils (assertUnparseList)

-- | Tests for unparsing 'MetaInfo' into COPS format and ARI format
unparseMetaInfoTests :: Test
unparseMetaInfoTests = TestList [unparseCopsMetaTests, unparseAriMetaTests]

-- | Tests for converting some example 'MetaInfo's to COPS format 'unparseCopsMetaInfo'
unparseCopsMetaTests :: Test
unparseCopsMetaTests = assertUnparseList testMeta (maybe "" show . unparseCopsMetaInfo)
  where
    testMeta :: [(MetaInfo, String, String)]
    testMeta = [(m, expected, label ++ " [COPS]") | (label, m, expected, _) <- testMetas]

-- | Tests for converting some example 'MetaInfo's to ARI format using 'unparseAriMetaInfo'
unparseAriMetaTests :: Test
unparseAriMetaTests = assertUnparseList testMeta (maybe "" show . unparseAriMetaInfo)
  where
    testMeta :: [(MetaInfo, String, String)]
    testMeta = [(m, expected, label ++ " [ARI]") | (label, m, _, expected) <- testMetas]

------------------------
--- Test data ----------
------------------------

-- | Example TRS signatures for testing along with expected output results.
-- Consists of tuples @(test label, sig, rules corresponding to sig, COPS format, ARI format)@
testMetas :: [(String, MetaInfo, String, String)]
testMetas =
  [("Unparse empty MetaInfo", emptyMetaInfo, "", "")]
