-- |
-- Module      : Test.Parse.Spec
-- Description : Parsing tests
--
-- This module collects tests cases for parsing problems from a given input format into
-- the internal Haskell representation.
module Test.Parse.Spec (parsingTests) where

import Test.HUnit
import Test.Parse.MetaInfo (parseMetaInfoTests)
import Test.Parse.Mstrs.Ari (parseAriMstrsTests)
import Test.Parse.Mstrs.Cops (parseCopsMstrsTests)
import Test.Parse.Mstrs.MsSig (parseMsSigTests)
import Test.Parse.Rule (parseRuleTests)
import Test.Parse.Term (parseTermTests)
import Test.Parse.Trs.Ari (parseAriTests)
import Test.Parse.Trs.Cops (parseCopsTests)
import Test.Parse.Trs.Sig (parseSigTests)

-- | Collect and run tests for parsing
parsingTests :: IO ()
parsingTests = do
  putStrLn "Runnning parsing tests"
  _ <-
    runTestTT $
      TestList
        [ parseTermTests,
          parseMetaInfoTests,
          parseRuleTests,
          parseSigTests,
          parseMsSigTests,
          parseCopsTests,
          parseAriTests,
          parseCopsMstrsTests,
          parseAriMstrsTests
        ]
  putStrLn "---"