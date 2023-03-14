-- |
-- Module      : Test.Unparse.Spec
-- Description : Unparsing tests
--
-- This module collects tests cases for unparsing problems from the internal representation into a
-- chosen output format.
module Test.Unparse.Spec (unparsingTests) where

import Test.HUnit
import Test.Unparse.Problem.MetaInfo (unparseMetaInfoTests)
import Test.Unparse.Problem.MsSig (unparseMsSigTests)
import Test.Unparse.Problem.Rule (unparseRuleTests)
import Test.Unparse.Problem.Term (unparseTermTests)
import Test.Unparse.Problem.TrsSig (unparseSigTests)
import Test.Unparse.UnparseMsTrs (unparseMsTrsTests)
import Test.Unparse.UnparseTrs (unparseTrsTests)

-- | Collect and run tests for unparsing
unparsingTests :: IO ()
unparsingTests = do
  putStrLn "Runnning unparsing tests"
  _ <-
    runTestTT $
      TestList
        [ unparseTermTests,
          unparseSigTests,
          unparseMsSigTests,
          unparseMetaInfoTests,
          unparseRuleTests,
          unparseTrsTests,
          unparseMsTrsTests
        ]
  putStrLn "---"
