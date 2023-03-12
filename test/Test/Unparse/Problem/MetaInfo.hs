-- |
-- Module      : Test.Unparse.Problem.MetaInfo
-- Description : Unparsing tests for TRS MetaInfo
--
-- This module defines test cases for unparsing 'MetaInfo's to COPS and ARI format
module Test.Unparse.Problem.MetaInfo (unparseMetaInfoTests) where

import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo)
import Data.Conversion.Unparse.Problem.MetaInfo (unparseAriMetaInfo, unparseCopsMetaInfo)
import Test.HUnit
import Test.Unparse.Utils (assertUnparseList)

-- | Tests for unparsing 'MetaInfo' into COPS format and ARI format
unparseMetaInfoTests :: Test
unparseMetaInfoTests = TestLabel "unparseMetaInfoTests" $ TestList [unparseCopsMetaTests, unparseAriMetaTests]

-- | Tests for converting some example 'MetaInfo's to COPS format 'unparseCopsMetaInfo'
unparseCopsMetaTests :: Test
unparseCopsMetaTests = assertUnparseList testMeta (show . unparseCopsMetaInfo)
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
-- Consists of tuples @(test label, sig, rules corresponding to @sig, COPS format, ARI format)@
testMetas :: [(String, MetaInfo, String, String)]
testMetas =
  [ ("Unparse empty MetaInfo", emptyMetaInfo, "", ""),
    ("Unparse a single comment", emptyMetaInfo {comment = Just "One comment"}, "(COMMENT \nOne comment)", "(meta-info (comment \"One comment\"))"),
    ("Unparse doi", emptyMetaInfo {doi = Just "10.1007/11805618_6"}, "(COMMENT \ndoi:10.1007/11805618_6)", "(meta-info (doi \"10.1007/11805618_6\"))"),
    ( "Unparse comment and origin",
      emptyMetaInfo {comment = Just "comment (with parentheses)", origin = Just "COPS #20"},
      "(COMMENT \ncomment (with parentheses)\norigin: COPS #20)",
      "(meta-info (origin \"COPS #20\"))\n(meta-info (comment \"comment (with parentheses)\"))"
    ),
    ( "Unparse all MetaInfo fields",
      emptyMetaInfo {comment = Just "[7] Example 2", doi = Just "10.1007/11805618_6", origin = Just "COPS #20", submitted = Just ["Takahito Aoto", "Junichi Yoshida", "Yoshihito Toyama"]},
      "(COMMENT \ndoi:10.1007/11805618_6\n[7] Example 2\norigin: COPS #20\nsubmitted by: Takahito Aoto, Junichi Yoshida, Yoshihito Toyama)",
      "(meta-info (origin \"COPS #20\"))\n(meta-info (doi \"10.1007/11805618_6\"))\n(meta-info (comment \"[7] Example 2\"))\n(meta-info (submitted \"Takahito Aoto\" \"Junichi Yoshida\" \"Yoshihito Toyama\"))"
    )
  ]
