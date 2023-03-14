-- | -- Module      : Test.Unparse.Utils
--  Description : Utils for unparsing tests
--
--  This module defines helper functions for testing unparsing functions
--  with HUnit.
module Test.Unparse.Utils (assertUnparseList, assertUnparse) where

import Prettyprinter (Doc)
import Test.HUnit

-- | Assert that a value @val :: a@ is unparsed to a string @expected@ using
-- unparser function @unparser :: (a -> Either String (Doc ann)@.
--
-- Prints an error message if unparsing fails or if a different result is obtained.
assertUnparse :: Show a => a -> (a -> Either String (Doc ann)) -> String -> Assertion
assertUnparse val unparser expected = case unparser val of
  Left err -> assertFailure err
  Right res -> assertEqual (show val ++ " not parsed correctly") expected (show res)

-- | Assert that each value in a list is correctly unparsed and wraps in a
-- 'TestList'.
--
-- Takes a list @xs@ of @(value to unparse, expected result, test label)@ tuples
-- and an unparser function @p@ and asserts that @p@ unparses every value in
-- @xs@ to the expected value using 'assertUnparse'.
assertUnparseList :: Show a => [(a, String, String)] -> (a -> Either String (Doc ann)) -> Test
assertUnparseList xs p =
  TestList
    [ TestLabel label (TestCase tc)
      | (val, expected, label) <- xs,
        let tc = assertUnparse val p expected
    ]
