-- |
-- Module      : Test.Parse.MetaInfo
-- Description : Parsing tests for MetaInfo
--
-- This module defines test cases for functions used to parse TRS meta-information (e.g. comments, authors, etc.).
module Test.Parse.MetaInfo (metaInfoParsingTests) where

import Data.Conversion.Parser.Parse.Problem.MetaInfo (parseAriMetaInfo, parseCopsMetaInfo)
import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo)
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)

-- | Tests for parsing rules in COPS format, including tests for which parsing should succeed and for which parsing should fail
metaInfoParsingTests :: Test
metaInfoParsingTests = TestList [parseAriMetaTests, badAriMetaTests, parseCopsMetaTests]

-- | Test cases for which 'parseAriMetaInfo' should succeed and produce the given output
parseAriMetaTests :: Test
parseAriMetaTests = assertParseList validMetaInfo parseAriMetaInfo
  where
    validMetaInfo :: [(String, MetaInfo)]
    validMetaInfo =
      [ ("(meta-info (origin \"COPS #20\"))", emptyMetaInfo {origin = Just "COPS #20"}),
        ("(meta-info ( doi \"10.1007/11805618_6\"  )  )", emptyMetaInfo {doi = Just "10.1007/11805618_6"}),
        ("(meta-info (comment \"C1\"))\n(meta-info (comment \"C2\"))", emptyMetaInfo {comments = Just ["C1", "C2"]}),
        ("(meta-info (submitted \"Person 1\" \"Person-2\"))", emptyMetaInfo {submitted = Just ["Person 1", "Person-2"]}),
        ( "(meta-info (origin \"COPS #20\"))\
          \ (meta-info (comment \"C1\"))\
          \ (meta-info (submitted   \"Person 1\")) \
          \ (meta-info (comment \"\"))",
          emptyMetaInfo
            { comments = Just ["C1", ""],
              origin = Just "COPS #20",
              submitted = Just ["Person 1"]
            }
        )
      ]

-- | Test cases for which 'parseAriMetaInfo' should fail
badAriMetaTests :: Test
badAriMetaTests = assertFailParseList badMetaInfo parseAriMetaInfo
  where
    badMetaInfo :: [String]
    badMetaInfo = []

-- | Test cases for which 'parseCopsMetaInfo' should succeed and produce the given output
parseCopsMetaTests :: Test
parseCopsMetaTests = assertParseList validMetaInfo parseCopsMetaInfo
  where
    validMetaInfo :: [(String, MetaInfo)]
    validMetaInfo = []
